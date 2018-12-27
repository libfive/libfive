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

#include "libfive/render/brep/simplex/simplex_tree.hpp"
#include "libfive/render/brep/simplex/simplex_neighbors.hpp"

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"

#include "../xtree.cpp"

namespace Kernel {

//  Here's our cutoff value (with a value set in the header)
template <unsigned N> constexpr double SimplexTree<N>::EIGENVALUE_CUTOFF;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
SimplexLeafSubspace<N>::SimplexLeafSubspace()
    : inside(false), index(0)
{
    /* (use default QEF constructor, which is all zeros) */
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
    surface.clear();
    std::fill(sub.begin(), sub.end(), nullptr);
}

template <unsigned N>
void SimplexLeaf<N>::releaseTo(Pool& object_pool)
{
    for (auto& s : sub) {
        if (--s->refcount == 0) {
            object_pool.next().put(s);
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
            typename SimplexTree<BaseDimension>::Leaf& leaf,
            const std::array<bool, ipow(3, BaseDimension)>& already_solved,
            const Region<BaseDimension>& region)
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
                    qef += leaf.sub[i]->qef.template sub<SubspaceFloating>();
                }
            }

            const auto r = region.template subspace<SubspaceFloating>();
            const auto sol = qef.solveBounded(r);
            error = sol.error;

            // Unpack from the reduced-dimension solution to the leaf vertex
            unsigned j = 0;
            for (unsigned i=0; i < BaseDimension; ++i) {
                if (SubspaceFloating & (1 << i)) {
                    leaf.sub[SubspaceIndex_]->vert(i) = sol.position(j++);
                } else if (SubspacePos & (1 << i)) {
                    leaf.sub[SubspaceIndex_]->vert(i) = region.upper(i);
                } else {
                    leaf.sub[SubspaceIndex_]->vert(i) = region.lower(i);
                }
            }

            assert(j == SubspaceDimension);
        }

        // Recurse!
        return std::max(error,
                Unroller<BaseDimension, SubspaceIndex_ - 1>()(
                    leaf, already_solved, region));
    }
};

// Terminate static unrolling
template <unsigned BaseDimension>
struct Unroller<BaseDimension, -1>
{
    double operator()(typename SimplexTree<BaseDimension>::Leaf&,
                    const std::array<bool, ipow(3, BaseDimension)>&,
                    const Region<BaseDimension>&)
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
    t->type = Interval::EMPTY;
    return std::move(t);
}

template <unsigned N>
Tape::Handle SimplexTree<N>::evalInterval(XTreeEvaluator* eval,
                                          Tape::Handle tape,
                                          const Region<N>& region,
                                          Pool& /* object_pool */)
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
        this->done();
    }
    return o.second;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void SimplexTree<N>::buildCornerQEFs(
        XTreeEvaluator* eval,
        Tape::Handle tape,
        const Region<N>& region,
        Pool& object_pool,
        const SimplexNeighbors<N>& neighbors,
        bool find_vertices)
{
    assert(this->leaf != nullptr);

    // Marks which subspaces have already been solved
    std::array<bool, ipow(3, N)> already_solved;
    std::fill(already_solved.begin(), already_solved.end(), false);

    // First, borrow solved QEF + vertex position + inside / outside
    // from our neighbors whenever possible.
    for (unsigned i=0; i < ipow(3, N); ++i) {
        const auto c = neighbors.check(NeighborIndex(i));
        if (c.first != nullptr) {
            this->leaf->sub[i] = c.first->sub[c.second.i];
            already_solved[i] = true;
        } else {
            object_pool.next().next().get(&this->leaf->sub[i]);
        }
        this->leaf->sub[i]->refcount++;
    }

    // First, we evaluate the corners, finding position + normal and storing
    // it in the corner QEFs (which are assumed to be empty)
    static_assert(ipow(2, N) < LIBFIVE_EVAL_ARRAY_SIZE,
                  "Too many points to evaluate");

    // Remap from a value in the range [0, count) to a corner index
    // in the range [0, 1 <<N).
    std::array<int, 1 << N> corner_indices;

    // Track how many corners have to be evaluated here
    // (if they have been be looked up from a neighbor, they don't have
    //  to be evaluated here, which can save time)
    size_t count = 0;

    // Pack values into the evaluator, skipping when values have already been
    // borrowed from neighbors
    for (unsigned i=0; i < ipow(2, N); ++i) {
        const auto sub = CornerIndex(i).neighbor();
        if (!already_solved[sub.i]) {
            eval->array.set(region.corner3f(i), count);
            corner_indices[count++] = i;
        }
    }

    // Then unpack into the corner QEF arrays (which are guaranteed to be
    // empty, because SimplexLeaf::reset() clears them).
    {
        const auto ds = eval->array.derivs(count, tape);
        const auto ambig = eval->array.getAmbiguous(count, tape);
        for (unsigned i=0; i < count; ++i) {
            const auto sub = CornerIndex(corner_indices[i]).neighbor();

            // Helper function to push a position + value + normal, swapping
            // the normal to an all-zeros vector if any items are invalid.
            auto push = [&](Eigen::Vector3f d) {
                Eigen::Matrix<double, N, 1> d_ =
                    d.template head<N>().template cast<double>();
                if (!d_.array().isFinite().all()) {
                    d_.array() = 0.0;
                }

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "==============================================\n";
                std::cout << region.corner(i).transpose() << " "
                          << d_.transpose() << " " << ds(3, i) << "\n";
#endif
                this->leaf->sub[sub.i]->qef.insert(
                        region.corner(corner_indices[i]), d_, ds(3, i));
            };

            // If this corner was ambiguous, then use the FeatureEvaluator
            // to get all of the possible derivatives, then add them to
            // the corner's QEF.
            if (ambig(i)) {
                const auto fs = eval->feature.features(
                        region.corner3f(corner_indices[i]), tape);
                for (auto& f : fs) {
                    push(f);
                }
            // Otherwise, use the normal found by the DerivArrayEvaluator
            } else {
                push(ds.col(i).template head<3>());
            }
        }
    }

    if (find_vertices) {
        // Statically unroll a loop to position every vertex within their subspace.
        Unroller<N, ipow(3, N) - 1>()(*this->leaf, already_solved, region);

        // Check whether each vertex is inside or outside
        saveVertexSigns(eval, tape, region, already_solved);
    }
}

template <unsigned N>
void SimplexTree<N>::evalLeaf(XTreeEvaluator* eval,
                              std::shared_ptr<Tape> tape,
                              const Region<N>& region,
                              Pool& object_pool,
                              const SimplexNeighbors<N>& neighbors)
{
    object_pool.next().get(&this->leaf);
    this->leaf->tape = tape;
    this->leaf->level = 0;

    // Build the corner-subspace QEFs by sampling the function at the corners
    buildCornerQEFs(eval, tape, region, object_pool, neighbors, true);

    // Check all subspace vertices to decide whether this leaf is
    // completely empty or full.  This isn't as conclusive as the
    // interval arithmetic, but if there were parts of the model
    // within the cell, we'd expect at least one vertex to hit them.
    bool all_inside = true;
    bool all_outside = true;
    for (unsigned i=0; i < ipow(3, N); ++i) {
        all_inside  &=   this->leaf->sub[i]->inside;
        all_outside &=  !this->leaf->sub[i]->inside;
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

    // We need to keep the QEF leaf, even if the region is completely
    // filled or empty, because it could contain data that's required
    // to solve for vertex positions when we merge this cell with a neighbor
    // that's partially occupied.

    this->done();
}

template <unsigned N>
bool SimplexTree<N>::collectChildren(XTreeEvaluator* eval,
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
    if (this->type == Interval::EMPTY || this->type == Interval::FILLED)
    {
        this->releaseChildren(object_pool);
        this->done();
        return true;
    }

    // Children may have been proven filled / empty by interval arithmetic
    // or by collapsing smaller cells.  In this case, they don't have QEF
    // data populated, so we add it here
    SimplexNeighbors<N> neighbors;
    const auto rs = region.subdivide();
    for (unsigned i=0; i < ipow(2, N); ++i) {
        if (cs[i]->leaf == nullptr) {
            assert(cs[i]->type == Interval::FILLED ||
                   cs[i]->type == Interval::EMPTY);

            object_pool.next().get(&cs[i]->leaf);
            cs[i]->buildCornerQEFs(eval, tape, rs[i], object_pool,
                                   neighbors.push(i, this->children),
                                   false);

            for (unsigned j=0; j < ipow(3, N); ++j) {
                cs[i]->leaf->sub[j]->inside = (cs[i]->type == Interval::FILLED);
            }
        }
    }

    // We've now passed all of our opportunitie to exit without
    // allocating a Leaf, so create one here.
    assert(this->leaf == nullptr);
    object_pool.next().get(&this->leaf);

    // Allocate SimplexLeafSubspace objects for this tree
    // TODO: can we pull from neighbors here as well?
    for (auto& s: this->leaf->sub) {
        object_pool.next().next().get(&s);
    }

    // Iterate over every child, collecting the QEFs and summing
    // them into larger QEFs.  To avoid double-counting, we skip
    // the low subspaces on high children, the cell marked with
    // an X adds every QEF marked with a *
    //
    //    -------------        -------------
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    *--*--*------        ---------*--*
    //    |     |     |        |     |     |
    //    *  X  *     |        |     |  X  *
    //    |     |     |        |     |     |
    //    *--*--*------        ---------*--*
    //
    //    ---------*--*        *--*--*------
    //    |     |     |        |     |     |
    //    |     |  X  *        *  X  *     |
    //    |     |     |        |     |     |
    //    -------------        -------------
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    -------------        -------------
    //
    //  Hopefully, the compiler optimizes this into a set of fixed
    //  assignments, rather than running through the loop.
    for (unsigned i=0; i < ipow(2, N); ++i) {
        assert(cs[i]->leaf != nullptr);

        for (unsigned j=0; j < ipow(3, N); ++j) {
            assert(cs[i]->leaf->sub[j] != nullptr);

            const auto child = CornerIndex(i);
            const auto neighbor = NeighborIndex(j);
            const auto fixed = neighbor.fixed<N>();
            const auto floating = neighbor.floating();
            const auto pos = neighbor.pos();

            // For every fixed axis, it must either be high,
            // or the child position on said axis must be low
            bool valid = true;
            for (unsigned d=0; d < N; ++d) {
                if (fixed & (1 << d)) {
                    valid &= (pos & (1 << d)) || (!(child.i & (1 << d)));
                }
            }

            if (!valid) {
                continue;
            }

            // Next, we need to figure out how to map the child's subspace
            // into the parent subspace.
            //
            // Every floating axis remains floating
            // Each fixed axis remains fixed if it agrees with the corner axis,
            // otherwise it's converted to a floating axis.
            uint8_t floating_out = 0;
            uint8_t pos_out = 0;

            for (unsigned d=0; d < N; ++d) {
                if (floating & (1 << d) ||
                   (pos & (1 << d)) != (child.i & (1 << d)))
                {
                    floating_out |= (1 << d);
                }
                else
                {
                    pos_out |= pos & (1 << d);
                }
            }
            const auto target = NeighborIndex::fromPosAndFloating(
                    pos_out, floating_out);

            this->leaf->sub[target.i]->qef += cs[i]->leaf->sub[j]->qef;
        }
    }

    // Statically unroll a loop to position every subspace vertex
    std::array<bool, ipow(3, N)> already_solved;
    std::fill(already_solved.begin(), already_solved.end(), false);
    const double err = Unroller<N, ipow(3, N) - 1>()(
            *this->leaf, already_solved, region);

    // We've successfully collapsed the cell!
    if (err < max_err) {
        // Store this tree's depth as a function of its children
        this->leaf->level = std::accumulate(
            cs.begin(), cs.end(), (unsigned)0,
            [](const unsigned& a, SimplexTree<N>* b)
            { return std::max(a, b->leaf->level);} ) + 1;

        // Store the tape for this larger region
        this->leaf->tape = tape;

        // Calculate and save vertex inside/outside states
        saveVertexSigns(eval, tape, region, already_solved);

        // TODO: reduce to interval here

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
        XTreeEvaluator* eval, Tape::Handle tape, const Region<N>& region,
        const std::array<bool, ipow(3, N)>& already_solved)
{
    // Finally, with every vertex positioned, solve for whether it is inside or outside.
    for (unsigned i=0; i < ipow(3, N); ++i)
    {
        // Skip subspaces that have already been solved
        if (already_solved[i]) {
            continue;
        }

        Eigen::Vector3f p;
        p << this->leaf->sub[i]->vert.template cast<float>().transpose(),
             region.perp.template cast<float>();

        // TODO: make this run in a single array evaluation, rather than
        // one per vertex.
        eval->array.set(p, 0);
        const auto out = eval->array.values(1, tape)[0];

        // TODO: make this case broader to deal with array vs non-array
        // evaluation (see comment at dc_tree.cpp:162).
        const bool inside = (out == 0)
            ? eval->feature.isInside(p, tape)
            : (out < 0);

        this->leaf->sub[i]->inside = inside;
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
uint32_t SimplexTree<N>::leafLevel() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->level;

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   return LEAF_LEVEL_INVALID;

        case Interval::UNKNOWN: assert(false);
    };
    return 0;
}

template <unsigned N>
void SimplexTree<N>::assignIndices() const
{
    uint64_t index = 1;
    SimplexNeighbors<N> neighbors;
    assignIndices(index, neighbors);
}

template <unsigned N>
void SimplexTree<N>::assignIndices(
        uint64_t& index, const SimplexNeighbors<N>& neighbors) const
{
    if (this->isBranch()) {
        assert(this->leaf == nullptr);
        for (unsigned i=0; i < this->children.size(); ++i) {
            auto new_neighbors = neighbors.push(i, this->children);
            this->children[i].load()->assignIndices(index, new_neighbors);
        }
    } else if (this->leaf != nullptr) {
        for (unsigned i=0; i < ipow(3, N); ++i) {
            auto n = neighbors.getIndex(i);
            if (n) {
                this->leaf->sub[i]->index = n;
            } else {
                this->leaf->sub[i]->index = index++;
            }
        }
    }
}

template <unsigned N>
void SimplexTree<N>::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        this->leaf->releaseTo(object_pool.next());
        this->leaf = nullptr;
    }
    object_pool.put(this);
}

}   // namespace Kernel
