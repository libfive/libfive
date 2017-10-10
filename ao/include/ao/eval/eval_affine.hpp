/*
Ao: a CAD kernel for modeling with implicit functions
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

#include "ao/eval/base.hpp"

namespace Kernel {

class AffineEvaluator : public BaseEvaluator
{
public:
    AffineEvaluator(std::shared_ptr<Tape> t);
    AffineEvaluator(std::shared_ptr<Tape> t,
                    const std::map<Tree::Id, float>& vars);

    /*
     *  Interval evaluation
     */
    Interval::I eval(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper);
    Interval::I evalAndPush(const Eigen::Vector3f& lower,
                            const Eigen::Vector3f& upper);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value);

protected:
    /*  f(clause) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, 1> f;

    /*  e(clause, j) is the jth epsilon for a particular clause */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> e;

    /*
     *  Collapse the affine expression into a single interval result
     */
    Interval::I i(size_t c) const;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*
     *  Converts the given interval to an affine expression
     *  (with a single error term), and store it for the given clause
     */
    void setAffine(Clause::Id id, Interval::I i);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
    friend class Tape; // for rwalk<IntervalEvaluator>
};

}   // namespace Kernel

