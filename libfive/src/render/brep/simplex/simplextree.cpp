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
SimplexTree<N>::SimplexTree(SimplexTree<N>* parent, unsigned index)
    : SimplexTree()
{
    reset(parent, index);
}

template <unsigned N>
SimplexTree<N>::SimplexTree()
{
    for (auto& c : children)
    {
        c.store(nullptr, std::memory_order_relaxed);
    }
    leaf = nullptr;
}

template <unsigned N>
void SimplexTree<N>::reset(SimplexTree<N>* p, unsigned i)
{
    parent = p;
    parent_index = i;
    type = Interval::UNKNOWN;

    // By design, a tree that is being reset must have no children
    for (auto& c : children)
    {
        assert(c.load() == nullptr);
        (void)c;
    }

    // By design, a tree that is being reset also has no leaf.
    assert(leaf == nullptr);

    pending.store((1 << N) - 1);
}

template <unsigned N>
SimplexTree<N>::Leaf::Leaf()
{
    reset();
}

template <unsigned N>
void SimplexTree<N>::Leaf::reset()
{
    level = 0;
    std::fill(index.begin(), index.end(), 0);
    tape.reset();
    surface.clear();
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

    type = Interval::state(o.first);
    if (!eval.isSafe())
    {
        type = Interval::AMBIGUOUS;
        return tape;
    }

    if (type == Interval::FILLED || type == Interval::EMPTY)
    {
        done();
    }
    return o.second;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned BaseDimension, unsigned SimplexIndex>
struct Unroller
{
    void operator()(typename SimplexTree<BaseDimension>::Leaf& leaf,
                    const CornerArray<BaseDimension>& corners,
                    const Region<BaseDimension>& region)
    {
        auto v = SimplexSolver::findVertex<BaseDimension, SimplexIndex>(
                corners, region);
        leaf.vertices.row(SimplexIndex) = v;

        // Recurse!
        Unroller<BaseDimension, SimplexIndex + 1>()(leaf, corners, region);
    }
};

// Terminate static unrolling
template <unsigned BaseDimension>
struct Unroller<BaseDimension, ipow(3, BaseDimension)>
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
void SimplexTree<N>::evalLeaf(XTreeEvaluator* eval,
                              const Region<N>& region, Tape::Handle tape,
                              ObjectPool<Leaf>& spare_leafs)
{
    CornerArray<N> corners;

    leaf = spare_leafs.get();
    Unroller<N, 0>()(*leaf, corners, region);

    for (unsigned i=0; i < ipow(3, N); ++i)
    {
        Eigen::Vector3f p;
        p << leaf->vertices.row(i).template cast<float>().transpose(),
             region.perp.template cast<float>();

        // TODO:  Pull corners from neighbors when possible
        eval->array.set(p, 0);
        const auto out = eval->array.values(1)[0];
        const bool inside = (out == 0)
            ? eval->feature.isInside(p)
            : (out < 0);

        leaf->inside[i] = inside;
        leaf->tape = tape;
        leaf->level = 0;
    }
    type = Interval::AMBIGUOUS; // TODO: check corners afterwards and collapse

    done();
}

template <unsigned N>
void SimplexTree<N>::releaseChildren(ObjectPool<SimplexTree>& spare_trees,
                                     ObjectPool<Leaf>& spare_leafs)
{
    for (auto& c : children)
    {
        auto ptr = c.exchange(nullptr);
        assert(ptr != nullptr);

        auto leaf = ptr->leaf;

        spare_trees.put(ptr);
        if (leaf != nullptr)
        {
            ptr->leaf = nullptr;
            spare_leafs.put(leaf);
        }
    }
}

template <unsigned N>
bool SimplexTree<N>::collectChildren(
        XTreeEvaluator* eval, Tape::Handle tape,
        double max_err, const Region<N>& region,
        ObjectPool<SimplexTree<N>>& spare_trees, ObjectPool<Leaf>& spare_leafs)
{
    // Wait for collectChildren to have been called N times
    if (pending-- != 0)
    {
        return false;
    }

    // Load the children here, to avoid atomics
    std::array<SimplexTree<N>*, 1 << N> cs;
    for (unsigned i=0; i < children.size(); ++i)
    {
        cs[i] = children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](SimplexTree<N>* o){ return o->isBranch(); }))
    {
        done();
        return true;
    }

    // Update corner and filled / empty state from children
    bool all_empty = true;
    bool all_full  = true;
    std::array<Interval::State, 1 << N> corners;
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        auto c = cs[i];
        assert(c != nullptr);

        all_empty &= (c->type == Interval::EMPTY);
        all_full  &= (c->type == Interval::FILLED);
    }

    type = all_empty ? Interval::EMPTY
         : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // If this cell is unambiguous, then forget all its branches and return
    if (type == Interval::FILLED || type == Interval::EMPTY)
    {
        releaseChildren(spare_trees, spare_leafs);
        done();
        return true;
    }

    // We've now passed all of our opportunities to exit without
    // allocating a Leaf, so create one here.
    assert(leaf == nullptr);
    leaf = spare_leafs.get();

    // TODO: attempt to collapse Leaf
    // For now, assume it failed
    {
        spare_leafs.put(leaf);
        leaf = nullptr;
    }

    done();
    return true;
}

template <unsigned N>
void SimplexTree<N>::done()
{
    if (parent)
    {
        assert(parent->children[parent_index].load() == nullptr);
        parent->children[parent_index].store(this, std::memory_order_relaxed);
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
unsigned SimplexTree<N>::level() const
{
    assert(!isBranch());
    switch (type)
    {
        case Interval::AMBIGUOUS:
            assert(leaf != nullptr);
            return leaf->level;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(leaf == nullptr);
                                return 0;
    };
    return 0;
}

template <unsigned N>
uint32_t SimplexTree<N>::leafLevel() const
{
    assert(!isBranch());
    switch (type)
    {
        case Interval::AMBIGUOUS:
            assert(leaf != nullptr);
            return leaf->level;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   return UINT32_MAX;
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
    if (isBranch()) {
        for (unsigned i=0; i < children.size(); ++i) {
            auto new_neighbors = neighbors.push(i, children);
            children[i].load()->assignIndices(index, new_neighbors);
        }
    } else if (leaf != nullptr) {
        for (unsigned i=0; i < leaf->index.size(); ++i) {
            auto n = neighbors.getIndex(i);
            if (n) {
                leaf->index[i] = n;
            } else {
                leaf->index[i] = index++;
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Explicit initialization of template
template class SimplexTree<2>;
template class SimplexTree<3>;

}   // namespace Kernel
