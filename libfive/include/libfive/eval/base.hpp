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
#pragma once
#include <Eigen/StdVector>

#include "libfive/eval/tape.hpp"

namespace Kernel {

template <typename T>
class BaseEvaluator
{
public:
    BaseEvaluator(std::shared_ptr<Tape> t,
                  const std::map<Tree::Id, float>& vs)
        : tape(t)
    {
        for (auto& v : t->vars)
        {
            auto itr = vs.find(v);
            if (itr == vs.end())
            {
                std::cerr << "BaseEvaluator::BaseEvaluator: "
                          << "uninitialized variable." << std::endl;
                vars.push_back(0.0f);
            }
            else
            {
                vars.push_back(itr->second);
            }
        }
    }

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value)
    {
        auto v = std::find(tape->vars.begin(), tape->vars.end(), var);
        if (v != tape->vars.end())
        {
            auto index = v - tape->vars.begin();
            bool changed = (vars.at(index) != value);
            vars.at(index) = value;
            return changed;
        }
        else
        {
            return false;
        }
    }

protected:
    std::shared_ptr<Tape> tape;

    /*  Storage for values assigned with set() */
    T x, y, z;

    /*  Cached variable values, to detect when they've changed */
    std::vector<float> vars;
};

}   // namespace Kernel
