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
    void reset(ProgressWatcher*) {}
    int64_t total_size() const { return 0; }
    ObjectPool<>& operator=(ObjectPool<>&&) { return *this; }
};

template <typename T, typename... Ts>
class ObjectPool<T, Ts...> : public ObjectPool<Ts...>
{
public:
    ObjectPool<T, Ts...>& operator=(ObjectPool<T, Ts...>&& other) {
        alloc = std::move(other.alloc);
        other.alloc.clear();
        d = other.d;
        next() = std::move(other.next());
        return *this;
    }

    template <typename... Args>
    void get(T** t, Args... args)
    {
        if (!d.size())
        {
            auto ptr = new T[N];
            alloc.push_back(ptr);
            for (unsigned i=0; i < N; ++i)
            {
                d.push(ptr + i);
            }
        }
        assert(d.size());
        *t = d.top();
        d.pop();

        assert(*t != nullptr);
        (*t)->reset(args...);
    }

    /*
     *  I'm not sure why these functions are necessary - I expected
     *  put() and get() to dispatch properly through the hierarchy.  Alas!
     */
    ObjectPool<Ts...>& next() {
        return *static_cast<ObjectPool<Ts...>*>(this);
    }
    const ObjectPool<Ts...>& next() const {
        return *static_cast<const ObjectPool<Ts...>*>(this);
    }

    void put(T* t)
    {
        assert(t != nullptr);
        d.push(t);
    }

    ~ObjectPool()
    {
        reset();
    }

    void claim(ObjectPool<T, Ts...>& other)
    {
        for (auto& t : other.alloc)
        {
            alloc.push_back(t);
        }
        other.alloc.clear();
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
        return (int64_t)alloc.size() * N - d.size();
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
    void reset(ProgressWatcher* progress_watcher=nullptr) {
        for (auto& t : alloc)
        {
            if (progress_watcher) progress_watcher->tick();
            delete [] t;
        }
        alloc.clear();
        next().reset(progress_watcher);
    }

private:
    /*  d stores available objects */
    std::stack<T*, std::vector<T*>> d;

    /*  alloc is the master list of allocated blocks */
    std::list<T*> alloc;

    static const unsigned N=512;
};

}   // namespace Kernel
