/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/vol/vol_tree.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/eval/evaluator.hpp"
#include "../xtree.cpp"

namespace libfive {

VolTree::VolTree(VolTree* parent, unsigned index, const Region<3>& r)
    : XTree<3, VolTree, void>(parent, index, r)
{
    // Nothing to do here
}

VolTree::VolTree()
    : XTree<3, VolTree, void>()
{
    // Nothing to do here
}

std::unique_ptr<VolTree> VolTree::empty()
{
    std::unique_ptr<VolTree> t(new VolTree);
    t->type = Interval::EMPTY;
    return t;
}

Tape::Handle VolTree::evalInterval(Evaluator* eval,
                                   const Tape::Handle& tape,
                                   const Region<3>& region,
                                   Pool&)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->intervalAndPush(
            region.lower3().template cast<float>(),
            region.upper3().template cast<float>(),
            tape);

    this->type = Interval::state(o.i);
    if (!o.safe)
    {
        this->type = Interval::AMBIGUOUS;
        this->tape = tape;
        return tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->done();
        if (tape != o.tape) {
            eval->getDeck()->claim(std::move(o.tape));
            return nullptr;
        }
    } else {
        this->tape = o.tape;
    }
    return o.tape;
}

void VolTree::evalLeaf(Evaluator* eval,
                       const Tape::Handle& tape,
                       const Region<3>& region,
                       Pool&, const VolNeighbors&)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->intervalAndPush(
            region.lower3().template cast<float>(),
            region.upper3().template cast<float>(),
            tape);

    this->type = Interval::state(o.i);
    if (!o.safe)
    {
        this->type = Interval::AMBIGUOUS;
        this->tape = tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->done();
        if (tape != o.tape) {
            eval->getDeck()->claim(std::move(o.tape));
        }
    } else {
        this->tape = o.tape;
    }
    this->done();
}

bool VolTree::collectChildren(Evaluator*,
                              const Tape::Handle&,
                              const Region<3>&,
                              Pool& object_pool,
                              double)
{
    // Wait for collectChildren to have been called N times
    if (this->pending-- != 0)
    {
        return false;
    }

    // Load the children here, to avoid atomics
    std::array<VolTree*, 8> cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](VolTree* o){ return o->isBranch(); }))
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
        all_empty &= (c->type == Interval::EMPTY);
        all_full  &= (c->type == Interval::FILLED);
    }

    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // If this cell is unambiguous, then forget all its branches and return
    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->releaseChildren(object_pool);
    }

    this->done();
    return true;
}

void VolTree::releaseTo(Pool& object_pool) {
    object_pool.put(this);
}

// Explicit instantiation of templates
template class XTree<3, VolTree, void>;

}   // namespace libfive
