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

namespace Kernel {

/*
 *  This is a simple object pool container, to avoid allocation churn.
 *
 *  It is not thread-safe; you should create one Pool per thread.
 *
 *  The target class must include a reset(...) function, which takes
 *  identical arguments as the constructor.
 */
template <typename T>
class Pool
{
public:
    template <typename... Args>
    T* get(Args... args)
    {
        if (d.size())
        {
            auto out = d.top();
            d.pop();
            out->reset(args...);
            return out;
        }
        else
        {
            return new T(args...);
        }
    }

    void put(T* t)
    {
        d.push(t);
    }

    ~Pool()
    {
        while (d.size())
        {
            delete d.top();
            d.pop();
        }
    }

protected:
    std::stack<T*, std::vector<T*>> d;
};

}   // namespace Kernel
