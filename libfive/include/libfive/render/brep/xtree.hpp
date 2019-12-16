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
    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit XTree();
    explicit XTree(T* parent, unsigned index, const Region<N>& region);

    /*
     *  Resets this tree to a freshly-constructed state
     */
    void reset(T* p, unsigned i, const Region<N>& r);

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

    /*  Optional leaf data, owned by a parent ObjectPool<Leaf> */
    L* leaf;

    /*  Marks whether this tree is fully constructed */
    mutable std::atomic_uint pending;

protected:
    /*
     *  Releases the children (and their Leaf pointers, if present)
     *  into the given object pools.
     *
     *  The template argument must be T::Pool, but that breaks
     *  template expansion.
     */
    template <typename Pool>
    void releaseChildren(Pool& object_pool);

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     *
     *  Returns true if instead it has installed a singleton into the parent's
     *  array, which happens if a tree type's hasSingletons() is true and the
     *  given tree is empty or filled.
     */
    bool done();
};

}   // namespace libfive
