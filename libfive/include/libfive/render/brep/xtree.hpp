/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>

#include "libfive/eval/interval.hpp"
#include "libfive/render/brep/region.hpp"

namespace libfive {

template <unsigned N, typename T, typename L>
class XTree
{
public:
    explicit XTree();
    explicit XTree(T* parent, unsigned index, const Region<N>& region);

    virtual ~XTree();

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const;

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const T* child(unsigned i) const;

    /*
     *  Walks the tree, resetting pending to its initial value of (1 << N) - 1
     */
    void resetPending() const;

    /*
     *  Sets type and calls done()
     */
    void setType(Interval::State t);

    /*  Parent tree, or nullptr if this is the root */
    T* parent;

    /*  Index into the parent tree's children array.  We only store the tree
     *  in the children array when it is complete, so it needs to know its
     *  index for when that time comes.  */
    unsigned parent_index;

    /*  Children pointers, if this is a branch  */
    std::array<std::atomic<T*>, 1 << N> children;

    /*  Leaf cell state, when known  */
    Interval::State type;

    /*  The cell's region */
    Region<N> region;

    /*  Optional leaf data */
    std::unique_ptr<L> leaf;

    /*  Marks whether this tree is fully constructed */
    mutable std::atomic_uint pending;

protected:
    /*
     *  Deletes all children
     */
    void freeChildren();

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     */
    void done();
};

}   // namespace libfive
