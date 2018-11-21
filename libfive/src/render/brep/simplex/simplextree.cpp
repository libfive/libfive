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
#include "libfive/render/brep/simplex/solver.hpp"
#include "libfive/render/brep/simplex/corner.hpp"

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"

namespace Kernel {

//  Here's our cutoff value (with a value set in the header)
template <unsigned N> constexpr double SimplexTree<N>::EIGENVALUE_CUTOFF;

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
SimplexLeaf<N>::SimplexLeaf()
{
    reset();
}

template <unsigned N>
void SimplexLeaf<N>::reset()
{
    level = 0;
    std::fill(index.begin(), index.end(), 0);
    tape.reset();
    surface.clear();

    for (auto& qef : qefs) {
        qef.reset();
    }
    level = 0;
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

template <unsigned BaseDimension, unsigned SimplexIndex_>
struct Unroller
{
    void operator()(typename SimplexTree<BaseDimension>::Leaf& leaf,
                    const CornerArray<BaseDimension>& corners,
                    const Region<BaseDimension>& region)
    {
        constexpr auto SimplexIndex = ipow(BaseDimension, 3) - SimplexIndex_;
        auto v = SimplexSolver::findVertex<BaseDimension, SimplexIndex>(
                corners, region);
        leaf.vertices.row(SimplexIndex) = v;

        // Recurse!
        Unroller<BaseDimension, SimplexIndex_ - 1>()(leaf, corners, region);
    }
};

// Terminate static unrolling
template <unsigned BaseDimension>
struct Unroller<BaseDimension, 0>
{
    void operator()(typename SimplexTree<BaseDimension>::Leaf&,
                    const CornerArray<BaseDimension>&,
                    const Region<BaseDimension>&)
    {
        // Nothing to do here
    }
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void SimplexTree<N>::evalLeaf(XTreeEvaluator* eval, const SimplexNeighbors<N>&,
                              const Region<N>& region, Tape::Handle tape,
                              ObjectPool<Leaf>& spare_leafs)
{
    CornerArray<N> corners;

    this->leaf = spare_leafs.get();

    // TODO:  Pull fully-evaluated QEFs from neighbors when possible

    // First, we evaluate the corners, finding position + normal and storing
    // it in the corner QEFs (which are assumed to be empty)
    static_assert(ipow(2, N) < LIBFIVE_EVAL_ARRAY_SIZE,
                  "Too many points to evaluate");
    // Pack values into the evaluator
    for (unsigned i=0; i < ipow(2, N); ++i) {
        eval->array.set(region.corner3f(i), i);
    }
    // Then unpack into the QEF arrays (which are guaranteed to be empty,
    // because SimplexLeaf::reset() clears them).
    const auto ds = eval->array.derivs(ipow(2, N));
    for (unsigned i=0; i < ipow(2, N); ++i) {
        const auto neighbor = CornerIndex(i).neighbor();
        this->leaf->qefs[neighbor.i].insert(
                region.corner(i),
                ds.col(i).template head<N>().template cast<double>(),
                ds(3, i));
    }

    Unroller<N, ipow(N, 3)>()(*this->leaf, corners, region);

    for (unsigned i=0; i < ipow(3, N); ++i)
    {
        Eigen::Vector3f p;
        p << this->leaf->vertices.row(i).template cast<float>().transpose(),
             region.perp.template cast<float>();

        eval->array.set(p, 0);
        const auto out = eval->array.values(1)[0];
        const bool inside = (out == 0)
            ? eval->feature.isInside(p)
            : (out < 0);

        this->leaf->inside[i] = inside;
        this->leaf->tape = tape;
        this->leaf->level = 0;
    }
    this->type = Interval::AMBIGUOUS; // TODO: check corners afterwards and collapse

    this->done();
}

template <unsigned N>
bool SimplexTree<N>::collectChildren(
        XTreeEvaluator* eval, Tape::Handle tape,
        double max_err, const Region<N>& region,
        ObjectPool<SimplexTree<N>>& spare_trees, ObjectPool<Leaf>& spare_leafs)
{
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
        this->releaseChildren(spare_trees, spare_leafs);
        this->done();
        return true;
    }

    // We've now passed all of our opportunities to exit without
    // allocating a Leaf, so create one here.
    assert(this->leaf == nullptr);
    this->leaf = spare_leafs.get();

    // TODO: attempt to collapse Leaf
    // For now, assume it failed
    {
        spare_leafs.put(this->leaf);
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
        for (unsigned i=0; i < this->leaf->index.size(); ++i) {
            auto n = neighbors.getIndex(i);
            if (n) {
                this->leaf->index[i] = n;
            } else {
                this->leaf->index[i] = index++;
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Explicit initialization of template
template class SimplexTree<2>;
template class SimplexTree<3>;

}   // namespace Kernel
