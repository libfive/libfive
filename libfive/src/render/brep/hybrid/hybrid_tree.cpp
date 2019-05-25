/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/eval/eval_xtree.hpp"
#include "libfive/eval/tape.hpp"

#include "libfive/render/brep/hybrid/hybrid_tree.hpp"
#include "libfive/render/brep/hybrid/hybrid_neighbors.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"
#include "libfive/render/brep/edge_tables.hpp"

#include "../xtree.cpp"

namespace Kernel {

template <unsigned N>
HybridTree<N>::HybridTree(HybridTree<N>* parent, unsigned index,
                          const Region<N>& r)
    : XTree<N, HybridTree<N>, HybridLeaf<N>>(parent, index, r)
{
    // Nothing to do here
}

template <unsigned N>
HybridTree<N>::HybridTree()
    : XTree<N, HybridTree<N>, HybridLeaf<N>>()
{
    // Nothing to do here
}

template <unsigned N>
std::unique_ptr<HybridTree<N>> HybridTree<N>::empty()
{
    std::unique_ptr<HybridTree> t(new HybridTree);
    t->type = Interval::UNKNOWN;
    return std::move(t);
}

template <unsigned N>
HybridLeaf<N>::HybridLeaf()
{
    reset();
}

template <unsigned N>
void HybridLeaf<N>::reset()
{
    std::fill(inside.begin(), inside.end(), false);
    std::fill(index.begin(), index.end(), 0);
    std::fill(intersection.begin(), intersection.end(), false);

    this->surface.clear();
    this->tape.reset();
    for (auto& q : qef) {
        q.reset();
    }
}

template <unsigned N>
Tape::Handle HybridTree<N>::evalInterval(XTreeEvaluator* eval,
                                         Tape::Handle tape,
                                         const Region<N>& region,
                                         Pool& object_pool)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->interval.evalAndPush(
            region.lower3().template cast<float>(),
            region.upper3().template cast<float>(),
            tape);

    this->type = Interval::state(o.first);
    if (!eval->interval.isSafe())
    {
        this->type = Interval::AMBIGUOUS;
        return tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        buildLeaf(eval, tape, region, object_pool);
        this->done();
    }
    return o.second;
}


template <unsigned N>
void HybridTree<N>::buildLeaf(XTreeEvaluator* eval,
                              std::shared_ptr<Tape> tape,
                              const Region<N>& region,
                              Pool& object_pool)
{
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->tape = tape;

    processCorners(eval, tape, region);
    processEdges(eval, tape, region);
    processSubspaces<2>(eval, tape, region);
    if (N == 3) {
        processSubspaces<3>(eval, tape, region);
    }
}

template <unsigned N>
void HybridTree<N>::processCorners(XTreeEvaluator* eval,
                                   Tape::Handle tape,
                                   const Region<N>& region)
{
    std::array<NeighborIndex, ipow(2, N)> targets;
    for (unsigned i=0; i < ipow(2, N); ++i) {
        eval->array.set(region.corner3f(i), i);
        targets[i] = CornerIndex(i).neighbor();
        this->leaf->pos.col(targets[i].i) = region.corner(i);

        // TODO: don't use the full-power evaluator when not needed
        this->leaf->inside[targets[i].i] = eval->feature.isInside(
                region.corner3f(i), tape);
    }
    // Store QEF data for this new vertex
    accumulate(eval, tape, targets.size(), targets.data());
}

template <unsigned N>
void HybridTree<N>::processEdges(XTreeEvaluator* eval,
                                 Tape::Handle tape,
                                 const Region<N>& region)
{
    for (const auto& e : EdgeTables<N>::edges) {
        const auto a = e.first.neighbor();
        const auto b = e.second.neighbor();
        assert(a.dimension() == 0);
        assert(b.dimension() == 0);

        // Find the edge's index
        const NeighborIndex edge = a | b;
        assert(edge.dimension() == 1);

        // If the edge doesn't have an obvious sign change,
        // then place a vertex on a sharp feature of the distance
        // field (which will hopefully be a sign change, if a sign
        // change exists along this edge).
        if (this->leaf->inside[a.i] == this->leaf->inside[b.i]) {
            // TODO: use the QEF here
            placeSubspaceVertex(eval,tape, region, edge,
                    (region.corner(e.first.i) +
                     region.corner(e.second.i)) / 2);
        }
        // Otherwise, do a binary search for the intersection
        else {
            Vec inside, outside;
            if (this->leaf->inside[a.i]) {
                inside = region.corner(e.first.i);
                outside = region.corner(e.second.i);
           } else if (this->leaf->inside[b.i]) {
                inside = region.corner(e.second.i);
                outside = region.corner(e.first.i);
            }
            placeSubspaceVertex(eval, tape, region, edge,
                    searchBetween(eval, tape, region, inside, outside));
        }
    }
}

template <unsigned N>
template<unsigned D>
void HybridTree<N>::processSubspaces(XTreeEvaluator* eval,
                                     Tape::Handle tape,
                                     const Region<N>& region)
{

    for (unsigned i=0; i < ipow(3, N); ++i) {
        NeighborIndex n(i);
        if (n.dimension() != D) {
            continue;
        }

        Vec center = Vec::Zero();
        unsigned count = 0;
        for (unsigned j=0; j < ipow(2, N); ++j) {
            if (n.contains(CornerIndex(j))) {
                center += region.corner(j);
                count++;
            }
        }
        placeSubspaceVertex(eval, tape, region, n, center / count);
    }
}

template <unsigned N>
void HybridTree<N>::placeSubspaceVertex(
        XTreeEvaluator* eval, Tape::Handle tape,
        const Region<N>& region,
        NeighborIndex n, const Vec& pos)
{
    // Store this edge's position
    this->leaf->pos.col(n.i) = pos;

    // Expensive check for inside / outsideness of this point (TODO)
    this->leaf->inside[n.i] = eval->feature.isInside<N>(pos, region, tape);

    // Check every edge from the new point to its neighbouring subspaces,
    // seeing whether there's a sign change and searching the edge
    // if that's the case
    bool found_sign_change = false;
    for (auto& t: EdgeTables<N>::neighbors[n.i]) {
        // If there isn't a sign change along this subspace-to-subspace
        // edge, then we shouldn't search it.
        assert(t.dimension() < n.dimension());
        if (this->leaf->inside[t.i] == this->leaf->inside[n.i])
        {
            continue;
        }

        found_sign_change = true;
        Vec inside, outside;
        if (this->leaf->inside[t.i]) {
            inside = this->leaf->pos.col(t.i);
            outside = this->leaf->pos.col(n.i);
        } else {
            inside = this->leaf->pos.col(n.i);
            outside = this->leaf->pos.col(t.i);
        }
        auto p = searchBetween(eval, tape, region, inside, outside);

        // Store the intersection point into the QEF
        eval->array.set<N>(p, region, 1);
        accumulate(eval, tape, 0, &n);
    }

    // If we didn't find a sign change, then store the vertex itself into
    // the subspace QEF.
    if (!found_sign_change) {
        eval->array.set<N>(pos, region, 0);
        accumulate(eval, tape, 1, &n);
    }
}

template <unsigned N>
typename HybridTree<N>::Vec HybridTree<N>::searchBetween(
        XTreeEvaluator* eval, Tape::Handle tape,
        const Region<N>& region,
        Vec inside, Vec outside)
{
    // Copied from simplex_mesher.cpp
    // TODO: unify all of these various implementations
    assert(tape.get() != nullptr);

    // There's an interesting question of precision + speed tradeoffs,
    // which mostly depend on how well evaluation scales in the
    // ArrayEaluator.  for now, we'll use the same value as XTree.
    constexpr int SEARCH_COUNT = 4;
    constexpr int POINTS_PER_SEARCH = 16;
    static_assert(POINTS_PER_SEARCH <= ArrayEvaluator::N,
                  "Overflowing ArrayEvaluator data array");

    // Multi-stage binary search for intersection
    for (int s=0; s < SEARCH_COUNT; ++s)
    {
        // Load search points into the evaluator
        Eigen::Array<double, N, POINTS_PER_SEARCH> ps;
        for (int j=0; j < POINTS_PER_SEARCH; ++j)
        {
                const double frac = j / (POINTS_PER_SEARCH - 1.0);
                ps.col(j) = (inside * (1 - frac)) + (outside * frac);
                eval->array.set<N>(ps.col(j), region, j);
        }

        auto out = eval->array.values(POINTS_PER_SEARCH, tape);

        // Skip one point, because the very first point is
        // already known to be inside the shape (but
        // sometimes, due to numerical issues, it registers
        // as outside!)
        for (unsigned j=1; j < POINTS_PER_SEARCH; ++j)
        {
            // We're searching for the first point that's outside of the
            // surface.  There's a special case for the final point in the
            // search, working around  numerical issues where different
            // evaluators disagree with whether points are inside or outside.
            if (out[j] > 0 || j == POINTS_PER_SEARCH - 1 ||
                (out[j] == 0 && !eval->feature.isInside<N>(
                    ps.col(j), region, tape)))
            {
                inside = ps.col(j - 1);
                outside = ps.col(j);
                break;
            }
        }
    }

    // TODO: we should weight the exact position based on values
    return (inside + outside) / 2;

}

template <unsigned N>
void HybridTree<N>::accumulate(XTreeEvaluator* eval,
                               Tape::Handle tape,
                               unsigned count,
                               NeighborIndex* target)
{
    auto ds = eval->array.derivs(count);
    auto ambig = eval->array.getAmbiguous(count);

    auto push = [&ds, &eval, &target, this](Eigen::Vector3f d, unsigned i) {
        Eigen::Matrix<double, N, 1> d_ =
            d.template head<N>().template cast<double>();
        if (!d_.array().isFinite().all()) {
            d_.array() = 0.0;
        }
        this->leaf->qef[target[i].i].insert(
                eval->array.get(i).template cast<double>()
                                  .template head<N>(),
                d_, ds(3, i));
    };

    for (unsigned i=0; i < count; ++i) {
        if (!ambig[i]) {
            push(ds.col(i).head<3>(), i);
        } else {
            const auto fs = eval->feature.features(
                    eval->array.get(i), tape);
            for (const auto& f : fs) {
                push(f, i);
            }
        }
    }

}

template <unsigned N>
void HybridTree<N>::evalLeaf(XTreeEvaluator* eval,
                             Tape::Handle tape,
                             const Region<N>& region,
                             Pool& object_pool,
                             const HybridNeighbors<N>& neighbors)
{
    (void)neighbors;

    buildLeaf(eval, tape, region, object_pool);

    bool all_empty = true;
    bool all_full  = true;
    for (unsigned i=0; i < ipow(3, N); ++i) {
        all_empty  &= !this->leaf->inside[i];
        all_full   &=  this->leaf->inside[i];
    }

    assert(this->type == Interval::UNKNOWN);
    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    this->done();
}

template <unsigned N>
bool HybridTree<N>::collectChildren(XTreeEvaluator* eval,
                                    Tape::Handle tape,
                                    const Region<N>& region,
                                    Pool& object_pool,
                                    double max_err)
{
    // Wait for collectChildren to have been called N times
    if (this->pending-- != 0)
    {
        return false;
    }

    // Load the children here, to avoid atomics
    std::array<HybridTree<N>*, 1 << N> cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](HybridTree<N>* o){ return o->isBranch(); }))
    {
        this->done();
        return true;
    }

    // Update filled / empty state from children
    bool all_empty = true;
    bool all_full  = true;
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        auto c = cs[i];
        assert(c != nullptr);

        all_empty &= (c->type == Interval::EMPTY);
        all_full  &= (c->type == Interval::FILLED);
    }

    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // If this cell is unambiguous, then forget all its branches and return
    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->releaseChildren(object_pool);
        buildLeaf(eval, tape, region, object_pool);
        this->done();
        return true;
    }

    // Eventually, we'll use these variables to perhaps collapse the tree
    (void)eval;
    (void)tape;
    (void)region;
    (void)max_err;

    this->done();
    return true;
}


template <unsigned N>
void HybridTree<N>::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }

    object_pool.put(this);
}

template <unsigned N>
uint32_t HybridTree<N>::leafLevel() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   // fallthrough
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            assert(this->region.level >= 0);
            return this->region.level;

        case Interval::UNKNOWN: return UINT32_MAX;
    };
    return 0;
}

/*  This helper struct lets us make a directed acyclic graph of
 *  tasks + neighbors, automatically cleaning them up when no one
 *  is using them anymore.  */
template <unsigned N>
struct AssignIndexTask {
    const HybridTree<N>* target;
    HybridNeighbors<N> neighbors;
    std::shared_ptr<AssignIndexTask<N>> parent;
};

template <unsigned N>
void HybridTree<N>::assignIndices(const BRepSettings& settings) const
{
    (void)settings; // TODO: multithreading and cancellation

    // We do a depth-first search here, flattened into a vector
    // to more easily convert to a multithreaded operation
    // (like the one in simplex_tree.cpp)
    using Task = std::shared_ptr<AssignIndexTask<N>>;
    std::stack<Task, std::vector<Task>> todo;
    auto task = std::make_shared<AssignIndexTask<N>>();
    task->target = this;
    todo.push(task);

    uint32_t index = 0;
    while (todo.size()) {
        const auto task = todo.top();
        todo.pop();

        // If this is a tree which can be subdivided, then push each
        // subtree as a new task.
        if (task->target->isBranch()) {
            for (unsigned i=0; i < task->target->children.size(); ++i) {
                const auto child = task->target->children[i].load();
                auto next = std::make_shared<AssignIndexTask<N>>();
                next->target = child;
                next->neighbors = task->neighbors.push(i, task->target->children);
                next->parent = task;
                todo.push(next);
            }
            continue;
        }

        // Try to assign a new value to every slot in the index array
        //
        // If the index is already assigned skip it.  Otherwise, spread the
        // value to every other index field that represents the same subspace.
        assert(task->target->leaf != nullptr);
        for (unsigned i_=0; i_ < ipow(3, N); ++i_)
        {
            // Skip if the index is already assigned
            if (task->target->leaf->index[i_] != 0) {
                continue;
            }

            // Record the new index here
            const auto my_index = ++index;
            task->target->leaf->index[i_] = my_index;

            // Functor to assign the new index to a given tree + neighbor index
            auto f = [my_index](const HybridTree<N>* t, NeighborIndex n) {
                assert(t->leaf->index[n.i] == 0 ||
                       t->leaf->index[n.i] == my_index);
                t->leaf->index[n.i] = my_index;
            };

            // Apply the functor to local neighbors
            NeighborIndex i(i_);
            task->neighbors.map(i, f);

            // We need to try walking up the tree, looking at the
            // neighbors of parent cells as long as they contain the target
            // vertex.  For example, in this situation:
            //
            //   -------------------------
            //   |           |           |
            //   |           |           |
            //   |           |           |
            //   ------------C------------
            //   |     |  X  |           |
            //   |-----|-----|           |
            //   |     |     |           |
            //   -------------------------
            //   we'd look at neighbors of X's parent cell to find corner C
            //
            //   On the other hand, in this situation:
            //   -------------------------
            //   |           |           |
            //   |           |           |
            //   |           |           |
            //   ------C------------------
            //   |  X  |     |           |
            //   |-----|-----|           |
            //   |     |     |           |
            //   -------------------------
            //   we don't want to back out to the parent cell of X, because
            //   the corner C isn't contained within that parent.
            //
            //   Each cell contains one vertex of the parent's corner cell,
            //   indicated by its parent_index variable.
            for (auto t = task; i.isCorner() && t->parent &&
                                t->target->parent_index == i.pos();
                 t = t->parent)
            {
                t->parent->neighbors.map(i, f);
            }
        }
    }
}

}   // namespace Kernel
