/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <stack>
#include <vector>
#include <list>
#include <cassert>


#include "libfive/render/brep/progress.hpp"

namespace Kernel {

/*
 *  This is a object pool container, to avoid allocation churn.
 *
 *  It uses a variadic template to contain pools for any number of
 *  different classes, and dispatches based on the first argument
 *  to get().
 *
 *  It owns all objects allocated under it, and allocates in blocks
 *  of N (by default 512).
 *
 *  It is not thread-safe; you should create one Pool per thread.
 *
 *  The target class must include a reset(...) function, and the
 *  constructor must take zero arguments.
 */
template <typename... T>
class ObjectPool
{
public:
    void claim(ObjectPool<>&) {}
    void reset(unsigned, ProgressHandler*) {}
    int64_t total_size() const { return 0; }
    ObjectPool<>& operator=(ObjectPool<>&&) { return *this; }
};

template <typename T, typename... Ts>
class ObjectPool<T, Ts...> : public ObjectPool<Ts...>
{
public:
    ObjectPool<T, Ts...>& operator=(ObjectPool<T, Ts...>&& other) {
        fresh_blocks = std::move(other.fresh_blocks);
        other.fresh_blocks.clear();

        allocated_blocks = std::move(other.allocated_blocks);
        other.allocated_blocks.clear();

        reusable_objects = std::move(other.reusable_objects);
        other.reusable_objects.clear();

        next() = std::move(other.next());
        return *this;
    }

    template <typename... Args>
    T* get(Args... args)
    {
        if (!reusable_objects.empty()) {
            auto out = reusable_objects.back();
            reusable_objects.pop_back();

            assert(out != nullptr);
            out->reset(args...);
            return out;
        }

        if (fresh_blocks.empty()) {
            fresh_blocks.push_back(std::make_pair(
                        static_cast<T*>(T::operator new[](sizeof(T) * N)), 0));
        }
        assert(fresh_blocks.size());

        auto& block = fresh_blocks.back();
        auto out = block.first + block.second;

        if (++block.second == N) {
            allocated_blocks.push_back(block.first);
            fresh_blocks.pop_back();
        }
        assert(out != nullptr);
        new (out) T(args...); // Placement new!
        return out;
    }

    ObjectPool<Ts...>& next() {
        return *static_cast<ObjectPool<Ts...>*>(this);
    }
    const ObjectPool<Ts...>& next() const {
        return *static_cast<const ObjectPool<Ts...>*>(this);
    }

    void put(T* t)
    {
        assert(t != nullptr);
        reusable_objects.push_back(t);
    }

    ~ObjectPool()
    {
        reset();
    }

    template <typename Q>
    static void claimVector(std::vector<Q>& mine, std::vector<Q>& other)
    {
        mine.reserve(mine.size() + other.size());
        for (auto& t : other) {
            mine.push_back(t);
        }
        other.clear();
    }

    void claim(ObjectPool<T, Ts...>& other)
    {
        claimVector(allocated_blocks, other.allocated_blocks);
        claimVector(fresh_blocks, other.fresh_blocks);
        claimVector(reusable_objects, other.reusable_objects);

        next().claim(other.next());
    }

    /*
     *  Returns the number of (assigned) items in the pool
     *  (ignoring items that have been allocated but not used).
     *
     *  This could be a negative number, if we we're storing available
     *  trees that were allocated by pools in different threads.
     */
    int64_t size() const
    {
        return (int64_t)(allocated_blocks.size() + fresh_blocks.size()) * N
               - reusable_objects.size();
    }

    int64_t total_size() const {
        return size() + next().total_size();
    }

    /*
     *  Deallocate everything stored in this object pool.
     *
     *  This may invalidate items stored in the available stack;
     *  get() must not be called after a pool is reset.
     *
     *  (this is the same as the destructor, but includes a progress callback)
     */
    void reset(unsigned workers=8,
               ProgressHandler* progress_watcher=nullptr)
    {
        auto workers_needed = std::max(allocated_blocks.size(),
                                       fresh_blocks.size());
        if (workers_needed < workers)
        {
            workers = workers_needed;
        }

        std::vector<std::future<void>> futures;
        futures.resize(workers);

        // Delete all of the blocks, using multiple threads for speed
        for (unsigned i=0; i < workers; ++i) {
            futures[i] = std::async(std::launch::async,
                    [i, this, workers, &progress_watcher]() {
                    for (unsigned j=i; j < allocated_blocks.size();
                                       j += workers)
                    {
                        for (unsigned k=0; k < N; ++k) {
                            allocated_blocks[j][k].~T();
                        }
                        if (progress_watcher) {
                            progress_watcher->tick();
                        }
                        T::operator delete[](allocated_blocks[j]);
                    }

                for (unsigned j=i; j < fresh_blocks.size(); j += workers) {
                    for (unsigned k=0; k < fresh_blocks[j].second; ++k) {
                        fresh_blocks[j].first[k].~T();
                    }
                    if (progress_watcher) {
                        progress_watcher->tick();
                    }
                    T::operator delete [](fresh_blocks[j].first);
                }
            });
        }

        // Wait on all of the futures
        for (auto& f : futures) {
            f.get();
        }

        allocated_blocks.clear();
        fresh_blocks.clear();

        next().reset(workers, progress_watcher);
    }

private:
    /*  Each fresh_block is a pointer to the start of the block,
     *  plus an offset (initially 0) with how many items in the block
     *  have already been used.  */
    std::vector<std::pair<T*, size_t>> fresh_blocks;

    /*  reused_objects stores available objects */
    std::vector<T*> reusable_objects;

    /*  allocated_blocks is the master list of allocated blocks,
     *  which must be fully used (otherwise they'd be in fresh_blocks) */
    std::vector<T*> allocated_blocks;

    static const unsigned N=512;
};

}   // namespace Kernel
