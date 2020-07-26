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
    : XTree(nullptr, 0, Region<N>())
{
    // Nothing to do here
}


template <unsigned N, typename T, typename L>
XTree<N, T, L>::XTree(T* parent, unsigned index, const Region<N>& region)
    : parent(parent), parent_index(index), type(Interval::UNKNOWN),
      region(region), leaf(nullptr), pending((1 << N) - 1)
{
    for (auto& c : children) {
        c.store(nullptr, std::memory_order_relaxed);
    }
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
XTree<N, T, L>::~XTree<N, T, L>() {
    freeChildren();
}

template <unsigned N, typename T, typename L>
void XTree<N, T, L>::freeChildren() {
    for (auto& c : children) {
        auto ptr = c.exchange(nullptr);
        delete ptr;
    }
}

template <unsigned N, typename T, typename L>
void XTree<N, T, L>::done()
{
    if (parent)
    {
        assert(parent->children[parent_index].load() == nullptr);
        T* t = static_cast<T*>(this);
        parent->children[parent_index].store(t, std::memory_order_relaxed);
    }
}

}   // namespace libfive
