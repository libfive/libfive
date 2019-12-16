/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <future>
#include <numeric>
#include <functional>
#include <limits>

#include <cmath>

#include <Eigen/StdVector>
#include <boost/lockfree/queue.hpp>
#include <boost/lockfree/stack.hpp>

#include "libfive/render/brep/simplex/simplex_tree.hpp"
#include "libfive/render/brep/simplex/simplex_neighbors.hpp"

#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"

#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"

#include "../xtree.inl"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
SimplexLeafSubspace<N>::SimplexLeafSubspace()
{
    reset();
}

template <unsigned N>
void SimplexLeafSubspace<N>::reset()
{
    inside = false;
    index = 0;
    vert.array() = 0.0;
    qef.reset();
    refcount.store(0);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
SimplexLeaf<N>::SimplexLeaf()
{
    reset();
}

template <unsigned N>
void SimplexLeaf<N>::reset()
{
    level = 0;
    tape.reset();
    std::fill(sub.begin(), sub.end(), nullptr);
    surface.clear();
}

template <unsigned N>
void SimplexLeaf<N>::releaseTo(Pool& object_pool)
{
    for (auto& s : sub) {
        auto sub = s.load();
        if (--sub->refcount == 0) {
            object_pool.next().put(sub);
        }
        s = nullptr;
    }
    object_pool.put(this);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned BaseDimension, int SubspaceIndex_>
struct Unroller
{
    /*
     *  Statically unrolls a loop to position each subspace's vertex,
     *  return the maximum error from the QEF solver.
     */
    double operator()(
            SimplexLeaf<BaseDimension>& leaf,
            const std::array<SimplexLeafSubspace<BaseDimension>*,
                       ipow(3, BaseDimension)>& leaf_sub,
            const std::array<bool, ipow(3, BaseDimension)>& already_solved,
            const Region<BaseDimension>& region,
            Evaluator* eval, const Tape::Handle& tape)
    {
        double error = 0.0;
        if (!already_solved[SubspaceIndex_]) {
            constexpr auto SubspaceIndex = NeighborIndex(SubspaceIndex_);
            constexpr unsigned SubspaceDimension = SubspaceIndex.dimension();
            constexpr unsigned SubspaceFloating = SubspaceIndex.floating();
            constexpr unsigned SubspacePos = SubspaceIndex.pos();

            // Collect all of the (non-inclusive) QEFs for this subspace
            QEF<SubspaceDimension> qef;
            for (unsigned i=0; i < ipow(3, BaseDimension); ++i) {
                if (SubspaceIndex.contains(NeighborIndex(i))) {
                    qef += leaf_sub[i]->qef.template sub<SubspaceFloating>();
                }
            }

            const auto r = region.template subspace<SubspaceFloating>();

#ifndef LIBFIVE_SIMPLEX_REFINE
#define LIBFIVE_SIMPLEX_REFINE 0
#endif
            for (unsigned i=0; i <= LIBFIVE_SIMPLEX_REFINE; ++i) {
                typename QEF<SubspaceDimension>::Solution sol;
#ifdef LIBFIVE_SIMPLEX_DC
                sol = qef.solveDC(r.center());
                if (!r.contains(sol.position, 0)) {
                    sol = qef.solveBounded(r);
                }
#else
                sol = qef.solveBounded(r);
#endif

                if (i == 0) {
                    error = sol.error;
                }

                // Unpack from the reduced-dimension solution to the leaf vertex
                unsigned j = 0;
                auto s = leaf_sub[SubspaceIndex_];
                for (unsigned i=0; i < BaseDimension; ++i) {
                    if (SubspaceFloating & (1 << i)) {
                        s->vert(i) = sol.position(j++);
                    } else if (SubspacePos & (1 << i)) {
                        s->vert(i) = region.upper(i);
                    } else {
                        s->vert(i) = region.lower(i);
                    }
                }
                assert(j == SubspaceDimension);

                // Evaluate the function at the solution point, and add that
                // information to the QEF, to do an iterative refinement.
                eval->set<BaseDimension>(s->vert, region, 0);
                const auto ds = eval->derivs(1, *tape);
                QEF<BaseDimension> extra;
                extra.insert(s->vert,
                             ds.col(0).template head<BaseDimension>().template cast<double>(),
                             ds(3, 0));

                qef /= 2.0;
                qef += extra.template sub<SubspaceFloating>();
            }
        }

        // Recurse!
        return std::max(error,
                Unroller<BaseDimension, SubspaceIndex_ - 1>()(
                    leaf, leaf_sub, already_solved, region,
                    eval, tape));
    }
};

// Terminate static unrolling
template <unsigned BaseDimension>
struct Unroller<BaseDimension, -1>
{
    double operator()(SimplexLeaf<BaseDimension>&,
                      const std::array<SimplexLeafSubspace<BaseDimension>*,
                                 ipow(3, BaseDimension)>&,
                      const std::array<bool, ipow(3, BaseDimension)>&,
                      const Region<BaseDimension>&,
                      Evaluator*, const Tape::Handle&)
    {
        return 0.0; // Nothing to do here
    }
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
SimplexTree<N>::SimplexTree(SimplexTree<N>* parent, unsigned index,
                            const Region<N>& region)
    : XTree<N, SimplexTree<N>, SimplexLeaf<N>>(parent, index, region)
{
    // Nothing to do here
}

template <unsigned N>
SimplexTree<N>::SimplexTree()
    : XTree<N, SimplexTree<N>, SimplexLeaf<N>>()
{
    // Nothing to do here
}

template <unsigned N>
std::unique_ptr<SimplexTree<N>> SimplexTree<N>::empty()
{
    std::unique_ptr<SimplexTree> t(new SimplexTree);
    t->type = Interval::UNKNOWN;

    // We set the parent to a silly invalid value, because it should
    // never be used (and if someone uses it by accident, we want to
    // crash quickly and with an obviously-wrong value).
    uintptr_t flag_ptr = 0xDEADBEEF;
    t->parent = reinterpret_cast<SimplexTree<N>*>(flag_ptr);

    return t;
}

template <unsigned N>
Tape::Handle SimplexTree<N>::evalInterval(Evaluator* eval,
                                          const Tape::Handle& tape,
                                          Pool& object_pool)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->intervalAndPush(
            this->region.lower3().template cast<float>(),
            this->region.upper3().template cast<float>(),
            tape);

    this->type = o.first.state();
    if (!o.first.isSafe())
    {
        assert(this->type == Interval::AMBIGUOUS);
        return tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        SimplexNeighbors<N> neighbors;

        this->leaf = object_pool.next().get();
        this->leaf->level = this->region.level;
        findLeafVertices(eval, tape, object_pool, neighbors);
        this->done();
    }
    return o.second;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void SimplexTree<N>::findLeafVertices(
        Evaluator* eval,
        const Tape::Handle& tape,
        Pool& object_pool,
        const SimplexNeighbors<N>& neighbors)
{
    assert(this->leaf != nullptr);

    // Marks which subspaces have already been solved
    std::array<bool, ipow(3, N)> already_solved;
    std::fill(already_solved.begin(), already_solved.end(), false);

    // First, borrow solved QEF + vertex position + inside / outside
    // from our neighbors whenever possible.
    for (unsigned i=0; i < ipow(3, N); ++i) {
        const auto c = neighbors.check(NeighborIndex(i));
        SimplexLeafSubspace<N>* s = nullptr;
        if (c.first != nullptr) {
            s = c.first->sub[c.second.i].load();
            already_solved[i] = true;
        } else {
            s = object_pool.next().next().get();
        }
        s->refcount++;
        this->leaf->sub[i].store(s);
    }

    const auto leaf_sub = getLeafSubs();

    //  This variable controls subsample size along each axis.  When it is 2,
    //  we only sample on the corners; when it is > 2, then we also sample
    //  between the corners to build up a denser grid within each cell.  This
    //  grows to the power of N (e.g. doubling it will cause 8x more points to
    //  be evaluated in the 3D case).
#ifndef LIBFIVE_SIMPLEX_SUBSAMPLE
#define LIBFIVE_SIMPLEX_SUBSAMPLE 0
#endif
    constexpr unsigned SAMPLES_PER_EDGE = 2 + LIBFIVE_SIMPLEX_SUBSAMPLE;
    static_assert(SAMPLES_PER_EDGE >= 2, "Too few samples per edge");
    static_assert(ipow(SAMPLES_PER_EDGE, N) < LIBFIVE_EVAL_ARRAY_SIZE,
                  "Too many points to evaluate");

    // Track how many grid points have to be evaluated here
    // (if they have been be looked up from a neighbor, they don't have
    //  to be evaluated here, which can save time)
    size_t count = 0;

    // Pack values into the evaluator, skipping when values have already been
    // borrowed from neighbors.  Here's a visual example of the packing,
    // with a subsampling factor of 4x:
    //
    //      X O O X
    //      O I I O
    //      O I I O
    //      X O O X
    //
    //  Values with an X are packed into each corner (4x)
    //  Values marked with O are packed into edges (4x)
    //  Values marked with I are packed into the volume (1x)
    //
    //  If nothing is borrowed from neighbors, we'd end up with
    //      X | X | O O | X | X | O O | O O | O O | I I I I
    //  in the evaluator (following the usual ternary ordering rule)
    Eigen::Array<double, N, ArrayEvaluator::N> positions;
    for (unsigned i=0; i < ipow(3, N); ++i) {
        const auto sub = NeighborIndex(i);
        if (!already_solved[sub.i]) {
            std::vector<Vec, Eigen::aligned_allocator<Vec>> todo;
            Vec init;
            for (unsigned a=0; a < N; ++a) {
                if (sub.isAxisFixed(a)) {
                    init(a) = (sub.axisPosition(a) ? this->region.upper
                                                   : this->region.lower)(a);
                }
            }
            todo.push_back(init);
            for (unsigned a=0; a < N; ++a) {
                if (!sub.isAxisFixed(a)) {
                    std::vector<Vec, Eigen::aligned_allocator<Vec>> next;
                    for (auto& t : todo) {
                        for (unsigned j=1; j < SAMPLES_PER_EDGE - 1; ++j) {
                            t(a) = (this->region.lower(a) * j +
                                    this->region.upper(a) * (SAMPLES_PER_EDGE - 1 - j))
                                     / (SAMPLES_PER_EDGE - 1);
                            next.push_back(t);
                        }
                    }
                    todo = next;
                }
            }
            assert(todo.size() == ipow(SAMPLES_PER_EDGE - 2, sub.dimension()));

            for (auto& t : todo) {
                positions.col(count) = t;
                eval->set<N>(t, this->region, count++);
            }
        }
    }

    // Then unpack into the subspace QEF arrays (which are guaranteed to be
    // empty, because SimplexLeaf::reset() clears them).
    {
        Eigen::Matrix<float, 4, ArrayEvaluator::N> ds;
        ds.leftCols(count) = eval->derivs(count, *tape);
        const auto ambig = eval->getAmbiguous(count, *tape);
        unsigned index=0;
        for (unsigned i=0; i < ipow(3, N); ++i) {
            const auto sub = NeighborIndex(i);
            if (!already_solved[sub.i]) {
                for (unsigned j=0;
                     j < ipow(SAMPLES_PER_EDGE - 2, sub.dimension());
                     ++j)
                {
                    // Helper function to push a position + value + normal,
                    // swapping the normal to an all-zeros vector if any
                    // items are invalid.  Visual Studio seems to have trouble
                    // capturing ds by default for some reason, so we capture
                    // all used variables individually.
                    auto push = [&ds, &leaf_sub, &positions, &sub, &index]
                        (Eigen::Vector3f d) {
                        Eigen::Matrix<double, N, 1> d_ =
                            d.template head<N>().template cast<double>();
                        if (!d_.array().isFinite().all()) {
                            d_.array() = 0.0;
                        }
                        leaf_sub[sub.i]->qef.insert(
                                positions.col(index),
                                d_, ds(3, index));
                    };

                    // If this corner was ambiguous, then use FeatureEvaluator
                    // to get all of the possible derivatives, then add them to
                    // the corner's QEF.
                    if (ambig(index)) {
                        const auto fs = eval->features<N>(
                                positions.col(index), this->region, tape);
                        for (auto& f : fs) {
                            push(f);
                        }
                    // Otherwise, use the normal found by the DerivArrayEvaluator
                    } else {
                        push(ds.col(index).template head<3>());
                    }
                    index++;
                }
            }
        }
    }

    // Statically unroll a loop to position every vertex within their subspace.
    Unroller<N, ipow(3, N) - 1>()(*this->leaf, leaf_sub,
                                  already_solved, this->region, eval, tape);


    // Check whether each vertex is inside or outside, either the hard way
    // (if this cell is ambiguous) or the easy way (if it's empty / filled).
    if (this->type == Interval::AMBIGUOUS) {
        saveVertexSigns(eval, tape, already_solved);
    } else {
        assert(this->type == Interval::FILLED ||
               this->type == Interval::EMPTY);
        for (auto& s : leaf_sub) {
            s->inside = (this->type == Interval::FILLED);
        }
    }
}

template <unsigned N>
void SimplexTree<N>::evalLeaf(Evaluator* eval,
                              const std::shared_ptr<Tape>& tape,
                              Pool& object_pool,
                              const SimplexNeighbors<N>& neighbors)
{
    this->leaf = object_pool.next().get();
    this->leaf->tape = tape;
    this->leaf->level = this->region.level;
    assert(this->region.level == 0);

    // Build the corner-subspace QEFs by sampling the function at the corners,
    // then solve for vertex position.
    this->type = Interval::AMBIGUOUS;
    findLeafVertices(eval, tape, object_pool, neighbors);
    checkVertexSigns();

    // We need to keep the leaf + QEF data, even if the region is completely
    // filled or empty, because it could contain data that's required
    // to solve for vertex positions when we merge this cell with a neighbor

    this->done();
}

template <unsigned N>
bool SimplexTree<N>::collectChildren(Evaluator* eval,
                                     const Tape::Handle& tape,
                                     Pool& object_pool,
                                     double max_err)
{
    // Wait for collectChildren to have been called N times
    if (this->pending-- != 0)
    {
        return false;
    }

    // Make a copy of the children pointers here, to avoid atomics
    std::array<SimplexTree<N>*, 1 << N> cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](SimplexTree<N>* o){ return o->isBranch(); }))
    {
        this->done();
        return true;
    }

    // We've now passed all of our opportunitie to exit without
    // allocating a Leaf, so create one here.
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();

    // Store this tree's depth and tape.  This could be delegated
    // until we're sure that we're keeping the leaf, but might as well
    // do it here (instead of having to write this code in both the
    // unambiguous and ambiguous cases).
    this->leaf->level = this->region.level;
    this->leaf->tape = tape;

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
    // (after solving for vertex positions).
    if (this->type == Interval::EMPTY || this->type == Interval::FILLED)
    {
        this->releaseChildren(object_pool);
        assert(!this->isBranch());

        SimplexNeighbors<N> neighbors;
        findLeafVertices(eval, tape, object_pool, neighbors);
        this->done();

        return true;
    }

    // Allocate SimplexLeafSubspace objects for this tree
    // TODO: can we pull from neighbors here as well?
    //
    // We use an array of plain pointers to reduce the number
    // of atomic loads that need to be performed.
    for (auto& s : this->leaf->sub) {
        auto ptr = object_pool.next().next().get();
        s.store(ptr);
        ptr->refcount++;
    }
    const auto leaf_sub = getLeafSubs();

    // Collect all of the child subspaces, based on a precalculated table
    for (unsigned i=0; i < ipow(3, N); ++i) {
        for (const auto& t: NeighborTables<N>::qefSumTable(i)) {
            leaf_sub[i]->qef += cs[t.first.i]->leaf->sub[t.second.i].load()->qef;
        }
    }

    // Statically unroll a loop to position every subspace vertex
    std::array<bool, ipow(3, N)> already_solved;
    std::fill(already_solved.begin(), already_solved.end(), false);
    const double err = Unroller<N, ipow(3, N) - 1>()(
            *this->leaf, leaf_sub, already_solved, this->region,
            eval, tape);

    // We've successfully collapsed the cell!
    if (err < max_err) {
        // Calculate and save vertex inside/outside states
        saveVertexSigns(eval, tape, already_solved);

        // Convert to EMPTY / FILLED if unambiguous
        checkVertexSigns();

        // Then, erase all of the children, which marks that this
        // cell is no longer a BRANCH.
        this->releaseChildren(object_pool);
        assert(!this->isBranch());
    } else {
        // Otherwise, this remains a branching node;
        // free this leaf back to the pool
        this->leaf->releaseTo(object_pool.next());
        this->leaf = nullptr;
    }

    this->done();
    return true;
}

template <unsigned N>
void SimplexTree<N>::saveVertexSigns(
        Evaluator* eval, const Tape::Handle& tape,
        const std::array<bool, ipow(3, N)>& already_solved)
{
    // With every vertex positioned, solve for whether it is inside or outside.
    assert(this->leaf != nullptr);
    assert(this->type == Interval::AMBIGUOUS);

    const auto leaf_sub = getLeafSubs();
    auto pos = [&leaf_sub, this](unsigned i) {
        Eigen::Vector3f p;
        p << leaf_sub[i]->vert.template cast<float>().transpose(),
             this->region.perp.template cast<float>();
        return p;
    };

    unsigned num = 0;
    for (unsigned i=0; i < ipow(3, N); ++i)
    {
        // Skip subspaces that have already been solved,
        // which include corners (since they have the solution
        // in their QEF data)
        if (already_solved[i] || NeighborIndex(i).isCorner()) {
            continue;
        }

        eval->set(pos(i), num++);
    }

    Eigen::Matrix<float, 1, ArrayEvaluator::N> values;
    values.leftCols(num) = eval->values(num, *tape);

    num = 0;
    for (unsigned i=0; i < ipow(3, N); ++i)
    {
        double out;
        // Skip subspaces that have already been solved
        if (already_solved[i]) {
            continue;
        }

        if (NeighborIndex(i).isCorner()) {
            out = leaf_sub[i]->qef.averageDistanceValue();
        } else {
            out = values[num++];
        }

        // Handle ambiguities with the high-power isInside check
        if (out == 0) {
            leaf_sub[i]->inside = eval->isInside(pos(i), tape);
        } else {
            leaf_sub[i]->inside = (out < 0);
        }
    }
}

template <unsigned N>
void SimplexTree<N>::checkVertexSigns() {
    // Check all subspace vertices to decide whether this leaf is
    // completely empty or full.  This isn't as conclusive as the
    // interval arithmetic, but if there were parts of the model
    // within the cell, we'd expect at least one vertex to hit them.
    bool all_inside = true;
    bool all_outside = true;
    for (const auto& s : this->leaf->sub) {
        auto sub = s.load();
        all_inside  &=   sub->inside;
        all_outside &=  !sub->inside;
    }

    // Store a tree type based on subspace vertex positions
    if (all_inside) {
        assert(!all_outside);
        this->type = Interval::FILLED;
    } else if (all_outside) {
        assert(!all_inside);
        this->type = Interval::EMPTY;
    } else {
        this->type = Interval::AMBIGUOUS;
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
std::array<SimplexLeafSubspace<N>*, ipow(3, N)>
SimplexTree<N>::getLeafSubs() const
{
    std::array<SimplexLeafSubspace<N>*, ipow(3, N)> out;
    for (unsigned i=0; i < out.size(); ++i) {
        out[i] = this->leaf->sub[i].load();
    }
    return out;
}

template <unsigned N>
uint32_t SimplexTree<N>::leafLevel() const
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

/*  This helper struct lets us make a directed acyclic graph
 *  of neighbors, automatically cleaning them up when no one
 *  is using them anymore.  */
template <unsigned N>
struct NeighborStack {
    SimplexNeighbors<N> ns;
    std::shared_ptr<NeighborStack<N>> parent;
};

template <unsigned N>
struct AssignIndexTask {
    const SimplexTree<N>* target;
    std::shared_ptr<NeighborStack<N>> neighbors;
};

template <unsigned N>
using LockFreeStack = boost::lockfree::stack<
        AssignIndexTask<N>,
        boost::lockfree::fixed_sized<true>>;

template <unsigned N>
void assignIndicesWorker(LockFreeStack<N>& tasks,
                         std::atomic<uint64_t>& index,
                         std::atomic_bool& done,
                         std::atomic_bool& cancel)
{
    // See detailed comments in worker_pool.cpp, which
    // implements a similar worker pool system.
    std::stack<AssignIndexTask<N>, std::vector<AssignIndexTask<N>>> local;

    while (!done.load() && !cancel.load()) {
        // Pick a task from the local empty, falling back to the MPMC
        // stack if the local stack is empty.  If both are empty, then
        // spin here until another thread adds a task to the global stack.
        AssignIndexTask<N> task;
        if (local.size()) {
            task = local.top();
            local.pop();
        } else if (!tasks.pop(task)) {
            task.target = nullptr;
        }

        if (task.target == nullptr) {
            continue;
        }

        // If this is a tree which can be subdivided, then push each
        // subtree as a new task.
        if (task.target->isBranch()) {
            for (unsigned i=0; i < task.target->children.size(); ++i) {
                AssignIndexTask<N> next_task;

                next_task.target = task.target->children[i].load();
                next_task.neighbors = std::make_shared<NeighborStack<N>>();
                next_task.neighbors->ns = task.neighbors->ns.push(i, task.target->children);
                next_task.neighbors->parent = task.neighbors;

                if (!tasks.bounded_push(next_task)) {
                    local.push(next_task);
                }
            }
            continue;
        }
        // Otherwise, do the actual work!

        assert(task.target->leaf != nullptr);
        // First, do a pass through the subspaces, converting them to
        // their canonical values by checking to see if a neighbor
        // has each subspace with a smaller pointer.
        for (unsigned i=0; i < ipow(3, N); ++i)
        {
            const auto i_ = NeighborIndex(i);
            const auto sub = task.target->leaf->sub[i].load();
            assert(sub != nullptr);

            // First, try to get it from a neighbor.  This function call also
            // walks down branching neighbors, to account for neighbors of
            // different levels, e.g.
            //   -------------------------
            //   |           |           |
            //   |     X     |           |
            //   |           |           |
            //   ------------C------------
            //   |     |  Z  |           |
            //   |-----Y-----|           |
            //   |     |     |           |
            //   -------------------------
            //   If we're in cell X and looking for corner C, then our neighbor
            //   Y should recurse into cell Z to check C's index within Z.
            auto new_sub = task.neighbors->ns.getSubspace(i_);
            if (reinterpret_cast<uintptr_t>(new_sub) >
                reinterpret_cast<uintptr_t>(sub))
            {
                assert(new_sub->inside == sub->inside);
                task.target->leaf->sub[i].store(new_sub);
            }

            // Otherwise, we need to try walking up the tree, looking at the
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
            //   we'd look at the neighbors of X's parent cell to find corner C
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
            if (i_.isCorner()) {
                auto t = task.target;
                auto neighbors_above = task.neighbors;
                while (t && t->parent &&
                       t->parent_index == i_.pos())
                {
                    t = t->parent;
                    neighbors_above = neighbors_above->parent;
                    auto new_sub = neighbors_above->ns.getSubspace(i_);
                    if (reinterpret_cast<uintptr_t>(new_sub) >
                        reinterpret_cast<uintptr_t>(sub))
                    {
                        assert(new_sub->inside == sub->inside);
                        task.target->leaf->sub[i].store(new_sub);
                    }
                }
            }
        }

        // Then, go through and make sure all indexes are assigned
        for (unsigned i=0; i < ipow(3, N); ++i)
        {
            auto sub = task.target->leaf->sub[i].load();

            // We do two atomic operations here:
            //
            // First, we compare to see if it's zero, assigning it to 1
            // if it was zero.  This prevents more than one thread from
            // entering the conditional block at once.
            //
            // Second, we assign the true index value (incrementing the
            // global index counter).
            uint64_t zero = 0;
            if (sub->index.compare_exchange_strong(zero, 1)) {
                sub->index.store(index++);
            }
        }

        SimplexTree<N>* t = nullptr;
        for (t = task.target->parent; t && t->pending-- == 0; t = t->parent)
        {
            // Walk up the tree here!
        }

        if (t == nullptr) {
            break;
        }
    }

    done.store(true);
}

template <unsigned N>
void SimplexTree<N>::assignIndices(const BRepSettings& settings) const
{
    this->resetPending();

    LockFreeStack<N> tasks(settings.workers);
    AssignIndexTask<N> first{this, std::make_shared<NeighborStack<N>>()};
    tasks.push(first);

    std::atomic<uint64_t> global_index(1);
    std::atomic_bool done(false);

    std::vector<std::future<void>> futures;
    futures.resize(settings.workers);
    for (unsigned i=0; i < settings.workers; ++i) {
        futures[i] = std::async(std::launch::async,
            [&done, &settings, &tasks, &global_index]() {
                assignIndicesWorker(tasks, global_index, done, settings.cancel);
            });
    }

    // Wait on all of the futures
    for (auto& f : futures) {
        f.get();
    }

    assert(done.load() || settings.cancel.load());
}

template <unsigned N>
void SimplexTree<N>::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        this->leaf->releaseTo(object_pool.next());
        this->leaf = nullptr;
    }
    object_pool.put(this);
}

}   // namespace libfive
