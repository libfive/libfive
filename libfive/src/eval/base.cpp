/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

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
#include <iostream>

#include "libfive/eval/base.hpp"

namespace Kernel {

BaseEvaluator::BaseEvaluator(std::shared_ptr<Tape> t,
                             const std::map<Tree::Id, float>& vars, int threadNo)
    : tape(t), threadNo(threadNo)
{
    for (auto& v : t->vars.right)
    {
        if (vars.find(v.first) == vars.end())
        {
            std::cerr << "BaseEvaluator::BaseEvaluator: "
                      << "uninitialized variable." << std::endl;
        }
    }
}

}   // namespace Kernel
