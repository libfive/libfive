/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/xtree.hpp"

namespace libfive {

template <unsigned N, typename T, typename L>
XTree<N, T, L>::XTree()
{
    for (auto& c : children)
    {
        c.store(nullptr, std::memory_order_relaxed);
    }
    leaf = nullptr;
}


template <unsigned N, typename T, typename L>
XTree<N, T, L>::XTree(T* parent, unsigned index, const Region<N>& region)
    : XTree()
{
    reset(parent, index, region);
}

template <unsigned N, typename T, typename L>
void XTree<N, T, L>::reset(T* p, unsigned i, const Region<N>& r)
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

template <unsigned N, typename T, typename L>
bool XTree<N, T, L>::isBranch() const {
    return children[0] != nullptr;
}

template <unsigned N, typename T, typename L>
const T* XTree<N, T, L>::child(unsigned i) const
{
    return isBranch()
        ? children[i].load(std::memory_order_relaxed)
        : static_cast<const T*>(this);
}

template <unsigned N, typename T, typename L>
void XTree<N, T, L>::resetPending() const
{
    pending.store((1 << N) - 1);
    if (isBranch()) {
        for (auto& c : children) {
            if (T::isSingleton(c.load())) {
                pending--;
            }
            c.load()->resetPending();
        }
    }
}

template <unsigned N, typename T, typename L>
void XTree<N, T, L>::setType(Interval::State t)
{
    type = t;
    done();
}

template <unsigned N, typename T, typename L>
template <typename Pool>
void XTree<N, T, L>::releaseChildren(Pool& object_pool)
{
    for (auto& c : children)
    {
        auto ptr = c.exchange(nullptr);
        assert(ptr != nullptr);
        ptr->releaseTo(object_pool);
    }
}

template <unsigned N, typename T, typename L>
bool XTree<N, T, L>::done()
{
    bool out = false;
    if (parent)
    {
        assert(parent->children[parent_index].load() == nullptr);
        T* t = static_cast<T*>(this);
        if (T::hasSingletons()) {
            if (this->type == Interval::EMPTY) {
                t = T::singletonEmpty();
                out = true;
            } else if (this->type == Interval::FILLED) {
                t = T::singletonFilled();
                out = true;
            }
        }
        parent->children[parent_index].store(t, std::memory_order_relaxed);
    }
    return out;
}

}   // namespace libfive
