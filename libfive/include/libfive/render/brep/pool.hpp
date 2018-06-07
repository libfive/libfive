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

#include <list>
#include <atomic>
#include <mutex>

namespace Kernel {
class Tree;
class XTreeEvaluator;

class Pool
{
public:
    Pool(const Tree& source, unsigned count);
    ~Pool();

    /*
     *  Returns an evaluator if any are available, otherwise nullptr
     */
    XTreeEvaluator* get();

    /*
     *  Returns this evaluator to the pool
     */
    void put(XTreeEvaluator* e);

    /*  Set this flag to cancel a computation */
    std::atomic_bool cancel;

protected:
    std::atomic_int count;
    std::mutex mut;
    std::list<XTreeEvaluator*> data;
};

}   // namespace Kernel
