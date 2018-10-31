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
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/region.hpp"

namespace Kernel {

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
    explicit XTree()
    {
        for (auto& c : children)
        {
            c.store(nullptr, std::memory_order_relaxed);
        }
        leaf = nullptr;
    }
    explicit XTree(T* parent, unsigned index, const Region<N>& region)
        : XTree()
    {
        reset(parent, index, region);
    }

    /*
     *  Resets this tree to a freshly-constructed state
     */
    void reset(T* p, unsigned i, Region<N> r)
    {
        parent = p;
        parent_index = i;
        type = Interval::UNKNOWN;
        region = r;

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

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const {
        return children[0] != nullptr;
    }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const T* child(unsigned i) const
    {
        return isBranch()
            ? children[i].load(std::memory_order_relaxed)
            : static_cast<const T*>(this);
    }

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
     */
    void releaseChildren(ObjectPool<T>& spare_trees,
                         ObjectPool<L>& spare_leafs)
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

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     */
    void done()
    {
        if (parent)
        {
            assert(parent->children[parent_index].load() == nullptr);
            parent->children[parent_index].store(static_cast<T*>(this),
                                                 std::memory_order_relaxed);
        }
    }
};

}   // namespace Kernel
