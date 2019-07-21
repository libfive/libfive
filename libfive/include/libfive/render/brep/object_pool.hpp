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

namespace libfive {
class ProgressHandler;

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
    int64_t num_blocks() const { return 0; }
    ObjectPool<>& operator=(ObjectPool<>&&) { return *this; }
};

template <typename T, typename... Ts>
class ObjectPool<T, Ts...> : public ObjectPool<Ts...>
{
public:
    ObjectPool<T, Ts...>& operator=(ObjectPool<T, Ts...>&& other);

    template <typename... Args>
    T* get(Args... args);

    ObjectPool<Ts...>& next();
    const ObjectPool<Ts...>& next() const;

    void put(T* t);

    ~ObjectPool();

    void claim(ObjectPool<T, Ts...>& other);

    /*
     *  Returns the number of (assigned) items in the pool
     *  (ignoring items that have been allocated but not used).
     *
     *  This could be a negative number, if we we're storing available
     *  trees that were allocated by pools in different threads.
     */
    int64_t size() const;
    int64_t num_blocks() const;

    /*
     *  Deallocate everything stored in this object pool.
     *
     *  This may invalidate items stored in the available stack;
     *  get() must not be called after a pool is reset.
     *
     *  (this is the same as the destructor, but includes a progress callback)
     */
    void reset(unsigned workers=8,
               ProgressHandler* progress_watcher=nullptr);

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

}   // namespace libfive
