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

#include "libfive/eval/eval_deriv_array.hpp"
#include "libfive/eval/eval_interval.hpp"
#include "libfive/eval/eval_feature.hpp"

namespace Kernel {

class XTreeEvaluator
{
public:
    XTreeEvaluator(const Tree t)
        : XTreeEvaluator(t, std::map<Tree::Id, float>())
    { /* Nothing to do here */ }

    XTreeEvaluator(const Tree t, const std::map<Tree::Id, float>& vars)
        : tape(new Tape(t)), array(tape, vars),
          interval(tape, vars), feature(tape, vars)
    { /* Nothing to do here */ }

    /*
     *  Updates variable values, return true if changed
     */
    bool updateVars(const std::map<Kernel::Tree::Id, float>& vars)
    {
        bool changed = false;
        for (auto& v : vars)
        {
            changed |= array.setVar(v.first, v.second);
            changed |= interval.setVar(v.first, v.second);
            changed |= feature.setVar(v.first, v.second);
        }
        return changed;
    }

protected:
    std::shared_ptr<Tape> tape;

public:
    DerivArrayEvaluator array;
    IntervalEvaluator interval;
    FeatureEvaluator feature;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel
