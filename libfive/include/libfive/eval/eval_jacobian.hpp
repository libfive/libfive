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

#include <Eigen/Eigen>

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_deriv.hpp"

namespace Kernel {

class JacobianEvaluator : public DerivEvaluator
{
public:
    JacobianEvaluator(std::shared_ptr<Tape> t);
    JacobianEvaluator(std::shared_ptr<Tape> t,
                      const std::map<Tree::Id, float>& vars);

    /*
     *  Returns the gradient with respect to all VAR nodes
     */
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
protected:
    /*
     *  Raw clause evaluation is done here!
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b, Clause::Id cond);

    /*  j(clause, var) = dclause / dvar */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> j;

    friend class Tape; // for rwalk<JacobianEvaluator>
};

}   // namespace Kernel
