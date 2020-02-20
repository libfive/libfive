/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/progress.hpp"
#include "libfive/render/brep/object_pool.hpp"

namespace libfive {

template <typename T, typename... Ts>
ObjectPool<T, Ts...>& ObjectPool<T, Ts...>::operator=(
    ObjectPool<T, Ts...>&& other)
{
    fresh_blocks = std::move(other.fresh_blocks);
    other.fresh_blocks.clear();

    allocated_blocks = std::move(other.allocated_blocks);
    other.allocated_blocks.clear();

    reusable_objects = std::move(other.reusable_objects);
    other.reusable_objects.clear();

    next() = std::move(other.next());
    return *this;
}

template <typename T, typename... Ts>
template <typename... Args>
T* ObjectPool<T, Ts...>::get(Args... args)
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

template <typename T, typename... Ts>
ObjectPool<Ts...>& ObjectPool<T, Ts...>::next() {
    return *static_cast<ObjectPool<Ts...>*>(this);
}

template <typename T, typename... Ts>
const ObjectPool<Ts...>& ObjectPool<T, Ts...>::next() const {
    return *static_cast<const ObjectPool<Ts...>*>(this);
}

template <typename T, typename... Ts>
void ObjectPool<T, Ts...>::put(T* t)
{
    assert(t != nullptr);
    reusable_objects.push_back(t);
}

template <typename T, typename... Ts>
ObjectPool<T, Ts...>::~ObjectPool()
{
    reset();
}

template <typename Q>
static void claimVector(
    std::vector<Q>& mine, std::vector<Q>& other)
{
    mine.reserve(mine.size() + other.size());
    for (auto& t : other) {
        mine.push_back(t);
    }
    other.clear();
}

template <typename T, typename... Ts>
void ObjectPool<T, Ts...>::claim(ObjectPool<T, Ts...>& other)
{
    claimVector(allocated_blocks, other.allocated_blocks);
    claimVector(fresh_blocks, other.fresh_blocks);
    claimVector(reusable_objects, other.reusable_objects);

    next().claim(other.next());
}

template <typename T, typename... Ts>
int64_t ObjectPool<T, Ts...>::size() const
{
    int64_t out = allocated_blocks.size() * N;
    for (const auto& f : fresh_blocks) {
        out += f.second;
    }
    return out - (int64_t)reusable_objects.size();
}

template <typename T, typename... Ts>
int64_t ObjectPool<T, Ts...>::num_blocks() const {
    return allocated_blocks.size() + fresh_blocks.size()
                                   + next().num_blocks();
}

template <typename T, typename... Ts>
void ObjectPool<T, Ts...>::reset(unsigned workers,
           ProgressHandler* progress_watcher)
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
}   // namespace libfive
