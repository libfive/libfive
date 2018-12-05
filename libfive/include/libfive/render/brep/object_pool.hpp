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
{};

template <typename T, typename... Ts>
class ObjectPool<T, Ts...> : public ObjectPool<Ts...>
{
public:
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

    void put(T* t)
    {
        assert(t != nullptr);
        d.push(t);
    }

    ~ObjectPool()
    {
        for (auto& t : alloc)
        {
            delete [] t;
        }
    }

    void release(std::list<T*>& out)
    {
        for (auto& t : alloc)
        {
            out.push_back(t);
        }
        alloc.clear();
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

private:
    /*  d stores available objects */
    std::stack<T*, std::vector<T*>> d;

    /*  alloc is the master list of allocated blocks */
    std::list<T*> alloc;

    static const unsigned N=512;
};

}   // namespace Kernel
