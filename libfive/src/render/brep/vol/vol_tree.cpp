/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/vol/vol_tree.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/eval/evaluator.hpp"
#include "../xtree.inl"

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
                                   Pool&)
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
        this->done();
        if (tape != o.second) {
            eval->getDeck()->claim(std::move(o.second));
            return nullptr;
        }
    }
    return o.second;
}

void VolTree::evalLeaf(Evaluator* eval,
                       const Tape::Handle& tape,
                       Pool&, const VolNeighbors&)
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
        this->type = Interval::AMBIGUOUS;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->done();
        if (tape != o.second) {
            eval->getDeck()->claim(std::move(o.second));
        }
    }
    this->done();
}

bool VolTree::collectChildren(Evaluator*,
                              const Tape::Handle&,
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

Interval::State VolTree::check(const Region<3>& r) const
{
    if (r.lower(0) >= region.lower(0) && r.upper(0) <= region.upper(0) &&
        r.lower(1) >= region.lower(1) && r.upper(1) <= region.upper(1) &&
        r.lower(2) >= region.lower(2) && r.upper(2) <= region.upper(2))
    {
        return this->type;
    }
    return Interval::UNKNOWN;
}

Interval::State VolTree::check(const Region<2>& r) const
{
    if (r.perp(0) >= region.lower(2) && r.perp(0) <= region.upper(2) &&
        r.lower(0) >= region.lower(0) && r.upper(0) <= region.upper(0) &&
        r.lower(1) >= region.lower(1) && r.upper(1) <= region.upper(1))
    {
        return this->type;
    }
    return Interval::UNKNOWN;
}

bool VolTree::contains(const Region<2>& r) const
{
    return (r.lower == region.lower.template head<2>()).all() &&
           (r.upper == region.upper.template head<2>()).all() &&
           r.perp(0) >= region.lower(2) && r.perp(0) <= region.upper(2);
}

bool VolTree::contains(const Region<3>& r) const
{
    return (r.lower == region.lower).all() &&
           (r.upper == region.upper).all();
}

const VolTree* VolTree::push(unsigned i, const Region<2>::Perp& perp) const
{
    if (isBranch()) {
        if (perp(0) >= region.center()(2)) {
            i |= Axis::Z;
        }
        return children[i].load();
    } else if (this->type == Interval::AMBIGUOUS) {
        return nullptr;
    } else {
        return this;
    }
}

const VolTree* VolTree::push(unsigned i, const Region<3>::Perp&) const
{
    if (isBranch()) {
        return children[i].load();
    } else if (this->type == Interval::AMBIGUOUS) {
        return nullptr;
    } else {
        return this;
    }
}

// Explicit instantiation of templates
template class XTree<3, VolTree, void>;

}   // namespace libfive
