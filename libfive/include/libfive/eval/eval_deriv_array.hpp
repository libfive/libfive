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

#include "libfive/eval/eval_array.hpp"

namespace Kernel {

class DerivArrayEvaluator : public ArrayEvaluator
{
public:
    DerivArrayEvaluator(std::shared_ptr<Tape> t, int threadNo);
    DerivArrayEvaluator(std::shared_ptr<Tape> t,
                        const std::map<Tree::Id, float>& vars,
                        int threadNo);

    /*  Non-virtual override; set() sets only values if the
    *  DerivArrayEvaluator is upcast to an ArrayEvaluator
    *  (or corresponding pointer/reference) first.
    */
    void set(const Eigen::Vector3f& p, size_t index);

    /*
    *  Storing the gradients lets us determine ambiguous members without
    *  calling getGradients again.  Calling getAmbiguous after upcasting will
    *  recalculate (and therefore can be used without calling the
    *  DerivArrayEvaluator version of set first.)  Calling getAmbiguous when
    *  some points were set using ArrayEvaluator::set has undefined result
    *  for those points, but does not affect the result for points that were
    *  set using DerivArrayEvaluator::set.
    */
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(size_t i);

protected:
    /*  d(clause).col(index) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<Eigen::Array<float, 3, N>, Eigen::Dynamic, 1> d;

    /*  Indicates the points at which each oracle is ambiguous; 
     *  this is set when calculating derivatives.
     */
    Eigen::Array<bool, Eigen::Dynamic, N> ambiguousOracles;

    /*  out(col) is a result [dx, dy, dz, w] */
    Eigen::Array<float, 4, N> out;

public:
    /*
     *  Single-point evaluation (return dx, dy, dz, distance)
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);

    /*
     *  Multi-point evaluation (values, and derivatives in case of nonlinear 
     *  oracles, must be stored with set) 
     */
    Eigen::Block<decltype(out), 4, Eigen::Dynamic> derivs(size_t count);

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<DerivArrayEvaluator>
};

}   // namespace Kernel


