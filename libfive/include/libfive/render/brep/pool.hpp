/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <stack>
#include <vector>
#include <list>

namespace Kernel {

/*
 *  This is a simple object pool container, to avoid allocation churn.
 *
 *  It owns all objects allocated under it, and allocates in blocks
 *  of N (by default 512).
 *
 *  It is not thread-safe; you should create one Pool per thread.
 *
 *  The target class must include a reset(...) function, and the
 *  constructor must take zero arguments.
 */
template <typename T>
class Pool
{
public:
    Pool(unsigned N=512) : N(N) { /* Nothing to do here */ }

    template <typename... Args>
    T* get(Args... args)
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
        auto out = d.top();
        d.pop();

        assert(out != nullptr);
        out->reset(args...);
        return out;
    }

    void put(T* t)
    {
        assert(t != nullptr);
        d.push(t);
    }

    ~Pool()
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
    ssize_t size() const
    {
        return (ssize_t)alloc.size() * N - d.size();
    }

protected:
    std::stack<T*, std::vector<T*>> d;
    std::list<T*> alloc;
    const unsigned N;
};

}   // namespace Kernel
