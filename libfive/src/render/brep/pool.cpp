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
#include "libfive/render/brep/pool.hpp"
#include "libfive/render/brep/eval_xtree.hpp"

namespace Kernel {

Pool::Pool(const Tree& tree, unsigned count)
    : cancel(false)
{
    for (unsigned i=0; i < count; ++i)
    {
        data.push_back(new XTreeEvaluator(tree));
    }
}

Pool::~Pool()
{
    std::lock_guard<std::mutex> lock(mut);
    while (data.size())
    {
        delete data.front();
        data.pop_front();
    }
}

XTreeEvaluator* Pool::get()
{
    std::lock_guard<std::mutex> lock(mut);
    if (data.size() > 0)
    {
        auto out = data.back();
        data.pop_back();
        return out;
    }
    else
    {
        return nullptr;
    }
}

void Pool::put(XTreeEvaluator* eval)
{
    std::lock_guard<std::mutex> lock(mut);
    data.push_back(eval);
}

}   // namespace Kernel
