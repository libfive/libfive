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
    t->type = Interval::EMPTY;
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
}

template <unsigned N>
Tape::Handle HybridTree<N>::evalInterval(XTreeEvaluator* eval,
                                         Tape::Handle tape,
                                         const Region<N>& region,
                                         Pool&)
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

template <unsigned N>
void HybridTree<N>::evalLeaf(XTreeEvaluator* eval,
                             Tape::Handle tape,
                             const Region<N>& region,
                             Pool& object_pool,
                             const HybridNeighbors<N>& neighbors)
{
    (void)eval;
    (void)tape;
    (void)region;
    (void)object_pool;
    (void)neighbors;
    this->type = Interval::EMPTY;
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

}   // namespace Kernel
