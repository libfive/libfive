/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/progress.hpp"

namespace libfive {

template <typename T>
class Root
{
public:
    Root() : ptr(nullptr) {}
    Root(T* ptr) : ptr(ptr) {}
    Root(Root&& other) { *this = std::move(other); }

    Root& operator=(Root&& other) {
        ptr = other.ptr;
        other.ptr = nullptr;
        object_pool = std::move(other.object_pool);
        tree_count = other.tree_count;
        return *this;
    }

    ~Root() { reset(BRepSettings()); }

    void reset(const BRepSettings& settings)
    {
        delete ptr;
        ptr = nullptr;
        if (settings.progress_handler) {
            settings.progress_handler->nextPhase(object_pool.num_blocks());
        }
        object_pool.reset(settings.workers, settings.progress_handler);
    }

    const T* operator->() const { return ptr; }
    const T* get() const { return ptr; }

    void claim(typename T::Pool& pool) {
        tree_count += pool.size();
        object_pool.claim(pool);
    }

    int64_t size() const { return tree_count; }

protected:
    T* ptr;
    typename T::Pool object_pool;

    // Used for progress tracking.  We use a signed value here because,
    // as we claim Pools of XTrees, it's possible for the intermediate
    // result to go negative (if one pool has claimed many trees from
    // another Pool, so it owns more trees than it has allocated).
    int64_t tree_count=0;
};

}   // namespace libfive
