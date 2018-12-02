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

#include "libfive/render/brep/simplex/simplextree.hpp"
#include "libfive/render/brep/simplex/simplex_neighbors.hpp"

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"

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
SimplexLeaf<N>::SimplexLeaf()
{
    reset();
}

template <unsigned N>
std::unique_ptr<SimplexTree<N>> SimplexTree<N>::empty()
{
    std::unique_ptr<SimplexTree> t(new SimplexTree);
    t->type = Interval::EMPTY;
    return std::move(t);
}

template <unsigned N>
void SimplexLeaf<N>::reset()
{
    level = 0;
    tape.reset();
    surface.clear();

    // TODO: can we pool these as well?
    for (auto& s : sub) {
        s.reset(new SimplexLeafSubspace<N>());
    }
}

template <unsigned N>
Tape::Handle SimplexTree<N>::evalInterval(
        IntervalEvaluator& eval, const Region<N>& region, Tape::Handle tape)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval.evalAndPush(
            region.lower3().template cast<float>(),
            region.upper3().template cast<float>(),
            tape);

    this->type = Interval::state(o.first);
    if (!eval.isSafe())
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

template <unsigned BaseDimension, int SubspaceIndex_>
struct Unroller
{
    void operator()(
            typename SimplexTree<BaseDimension>::Leaf& leaf,
            const std::array<bool, ipow(3, BaseDimension)>& already_solved,
            const Region<BaseDimension>& region)
    {
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
        Unroller<BaseDimension, SubspaceIndex_ - 1>()(
                leaf, already_solved, region);
    }
};

// Terminate static unrolling
template <unsigned BaseDimension>
struct Unroller<BaseDimension, -1>
{
    void operator()(typename SimplexTree<BaseDimension>::Leaf&,
                    const std::array<bool, ipow(3, BaseDimension)>&,
                    const Region<BaseDimension>&)
    {
        // Nothing to do here
    }
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void SimplexTree<N>::evalLeaf(XTreeEvaluator* eval,
                              const SimplexNeighbors<N>& neighbors,
                              const Region<N>& region, Tape::Handle tape,
                              ObjectPool<Leaf>& spare_leafs)
{
    spare_leafs.get(&this->leaf);
    this->leaf->tape = tape;
    this->leaf->level = 0;

    // Track how many corners have to be evaluated here
    // (if they can be looked up from a neighbor, they don't have
    //  to be evaluated here, which can save time)
    size_t count = 0;

    // Marks which subspaces have already been solved
    std::array<bool, ipow(3, N)> already_solved;
    std::fill(already_solved.begin(), already_solved.end(), false);

    // Remap from a value in the range [0, count) to a corner index
    // in the range [0, 1 <<N).
    std::array<int, 1 << N> corner_indices;

    // First, borrow solved QEF + vertex position + inside / outside
    // from our neighbors whenever possible.
    for (unsigned i=0; i < ipow(3, N); ++i) {
        const auto sub = NeighborIndex(i);
        const auto c = neighbors.check(sub);
        if (c.first != nullptr) {
            this->leaf->sub[sub.i] = c.first->sub[c.second.i];
            already_solved[sub.i] = true;
        }
    }

    // First, we evaluate the corners, finding position + normal and storing
    // it in the corner QEFs (which are assumed to be empty)
    static_assert(ipow(2, N) < LIBFIVE_EVAL_ARRAY_SIZE,
                  "Too many points to evaluate");
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

    // Statically unroll a loop to position every vertex within their subspace.
    // TODO: skip already-solved QEFs here somehow
    Unroller<N, ipow(N, 3) - 1>()(*this->leaf, already_solved, region);

    // Finally, with every vertex positioned, solve for whether it is inside or outside.
    for (unsigned i=0; i < ipow(N, 3); ++i)
    {
        // Skip subspaces that have already been solved
        if (already_solved[i]) {
            continue;
        }

        Eigen::Vector3f p;
        p << this->leaf->sub[i]->vert.template cast<float>().transpose(),
             region.perp.template cast<float>();

        eval->array.set(p, 0);
        const auto out = eval->array.values(1)[0];

        // TODO: make this case broader to deal with array vs non-array
        // evaluation (see comment at dc_tree.cpp:162).
        const bool inside = (out == 0)
            ? eval->feature.isInside(p)
            : (out < 0);

        this->leaf->sub[i]->inside = inside;
    }
    this->type = Interval::AMBIGUOUS; // TODO: check corners afterwards and collapse

    this->done();
}

template <unsigned N>
bool SimplexTree<N>::collectChildren(
        XTreeEvaluator* eval, Tape::Handle tape,
        double max_err, const Region<N>& region,
        Pool& object_pool)
{
    // TODO
    (void)eval;
    (void)tape;
    (void)max_err;
    (void)region;

    // Wait for collectChildren to have been called N times
    if (this->pending-- != 0)
    {
        return false;
    }

    // Load the children here, to avoid atomics
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
    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->releaseChildren(object_pool);
        this->done();
        return true;
    }

    // We've now passed all of our opportunities to exit without
    // allocating a Leaf, so create one here.
    assert(this->leaf == nullptr);
    object_pool.next().get(&this->leaf);

    // TODO: attempt to collapse Leaf
    // For now, assume it failed
    {
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }

    this->done();
    return true;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
unsigned SimplexTree<N>::level() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->level;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return 0;
    };
    return 0;
}

template <unsigned N>
uint32_t SimplexTree<N>::leafLevel() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->level;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   return LEAF_LEVEL_INVALID;
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
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }

    object_pool.put(this);
}
////////////////////////////////////////////////////////////////////////////////
// Explicit initialization of template
template class SimplexTree<2>;
template class SimplexTree<3>;

}   // namespace Kernel
