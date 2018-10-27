/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/progress.hpp"

namespace Kernel {

template <typename Tree>
class Root
{
public:
    Root() : ptr(nullptr) {}
    Root(Tree* ptr) : ptr(ptr) {}
    Root(Root&& other) { *this = std::move(other); }

    Root& operator=(Root&& other) {
        ptr = other.ptr;
        other.ptr = nullptr;
        trees = std::move(other.trees);
        leafs = std::move(other.leafs);
        tree_count = other.tree_count;
        return *this;
    }

    ~Root() { reset(); }

    void reset(ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK)
    {
        ptr = nullptr;

        std::atomic_bool done(false);
        std::atomic_bool cancel(false);
        auto progress_watcher = ProgressWatcher::build(
                trees.size() + leafs.size(), 2.0f,
                progress_callback, done, cancel);

        for (auto& t : trees)
        {
            if (progress_watcher) progress_watcher->tick();
            delete [] t;
        }
        for (auto& f : leafs)
        {
            if (progress_watcher) progress_watcher->tick();
            delete [] f;
        }
        done.store(true);
        delete progress_watcher;

        trees.clear();
        leafs.clear();
    }

    const Tree* operator->() { return ptr; }
    const Tree* get() const { return ptr; }

    void claim(ObjectPool<Tree>& pool) {
        tree_count += pool.size();
        pool.release(trees);
    }
    void claim(ObjectPool<typename Tree::Leaf>& pool) { pool.release(leafs); }

    int64_t size() const { return tree_count; }

protected:
    Tree* ptr;
    std::list<Tree*> trees;
    std::list<typename Tree::Leaf*> leafs;

    // Used for progress tracking.  We use a signed value here because,
    // as we claim Pools of XTrees, it's possible for the intermediate
    // result to go negative (if one pool has claimed many trees from
    // another Pool, so it owns more trees than it has allocated)..
    int64_t tree_count=0;
};

}   // namespace Kernel
