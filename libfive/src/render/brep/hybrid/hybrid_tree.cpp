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
#include "libfive/render/brep/neighbor_tables.hpp"

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
    this->level = 0;
    this->surface.clear();
    this->tape.reset();
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
        buildDummyLeaf(region, object_pool);
        this->done();
    }
    return o.second;
}

template <unsigned N>
void HybridTree<N>::buildDummyLeaf(const Region<N>& region,
                                   Pool& object_pool)
{
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->level = region.level;

    for (unsigned i=0; i < ipow(3, N); ++i) {
        Vec center = Vec::Zero();
        unsigned count = 0;
        for (unsigned j=0; j < ipow(2, N); ++j) {
            if (NeighborIndex(i).contains(CornerIndex(j))) {
                center += region.corner(j);
                count++;
            }
        }
        assert(count > 0);
        this->leaf->pos.col(i) = center / count;
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

    buildDummyLeaf(region, object_pool);
    bool all_empty = true;
    bool all_full  = true;

    for (unsigned i=0; i < ipow(3, N); ++i) {
        Eigen::Vector3f p;
        p << this->leaf->pos.col(i).template cast<float>(),
             region.perp.template cast<float>();
        this->leaf->inside[i] = eval->feature.isInside(p, tape);
        all_empty  &= !this->leaf->inside[i];
        all_full   &=  this->leaf->inside[i];
    }

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

    // Update corner and filled / empty state from children
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
            return this->leaf->level;

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
void HybridTree<N>::assignIndices() const
{
    // We do a depth-first search here, flattened into a vector
    // to more easily convert to a multithreaded operation
    // (like the one in simplex_tree.cpp)
    using Task = std::shared_ptr<AssignIndexTask<N>>;
    std::stack<Task, std::vector<Task>> todo;
    if (this->type == Interval::AMBIGUOUS) {
        auto task = std::make_shared<AssignIndexTask<N>>();
        task->target = this;
        todo.push(task);
    }

    uint32_t index = 0;
    while (todo.size()) {
        const auto task = todo.top();
        todo.pop();

        // If this is a tree which can be subdivided, then push each
        // subtree as a new task.
        if (task->target->isBranch()) {
            for (unsigned i=0; i < task->target->children.size(); ++i) {
                const auto child = task->target->children[i].load();
                if (child->type == Interval::AMBIGUOUS) {
                    auto next = std::make_shared<AssignIndexTask<N>>();
                    next->target = child;
                    next->neighbors = task->neighbors.push(i, task->target->children);
                    next->parent = task;
                    todo.push(next);
                }
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
                assert(t->leaf->index[n.i] == 0);
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
