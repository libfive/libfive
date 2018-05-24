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

#include "libfive/eval/eval_deriv.hpp"
namespace Kernel {

/* This helper class adds a base_derivs to the DerivEvaluator, for use in 
 * oracles that need to implement this capability by passing it on to tapes.
 */

class BaseDerivEvaluator : public DerivEvaluator
{
public:
    BaseDerivEvaluator(std::shared_ptr<Kernel::Tape> t)
      : DerivEvaluator(t) {}

    BaseDerivEvaluator(std::shared_ptr<Kernel::Tape> t,
      const std::map<Tree::Id, float>& vars)
      : DerivEvaluator(t, vars) {}

    // Non-virtual override, wrapping deriv for compatibility
    // with Tape::baseEval.
    Eigen::Vector4f eval(const Eigen::Vector3f& pt) {
        return deriv(pt);
    }

    Eigen::Vector4f baseDeriv(const Eigen::Vector3f& pt) {
        evalBase = true;
        auto out = tape->baseEval<BaseDerivEvaluator, 
                                  Eigen::Vector4f>(*this, pt);
        evalBase = false;
        return out;
    }
};



} //Namespace Kernel

