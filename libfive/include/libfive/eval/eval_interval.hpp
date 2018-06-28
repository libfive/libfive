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

#include "libfive/eval/base.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/eval/clause.hpp"

namespace Kernel {
class Tape; /* Forward declaration */

class IntervalEvaluator : public BaseEvaluator
{
public:
    IntervalEvaluator(std::shared_ptr<Deck> d);
    IntervalEvaluator(std::shared_ptr<Deck> d,
                      const std::map<Tree::Id, float>& vars);

    /*
     *  Interval evaluation
     */
    Interval::I eval(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper);
    Interval::I eval(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper,
                     std::shared_ptr<Tape> tape);

    std::pair<Interval::I, std::shared_ptr<Tape>> evalAndPush(
                     const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper);
    std::pair<Interval::I, std::shared_ptr<Tape>> evalAndPush(
                     const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper,
                     std::shared_ptr<Tape> tape);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value);

    bool isSafe() const { return safe; }

protected:
    /*  i[clause] is the interval result for that clause */
    std::vector<Interval::I> i;

    /*  Marks whether the most recent evaluation had any NaN or inf
     *  results (which can behave differently for intervals vs points */
    bool safe;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    friend class Tape; // for rwalk<IntervalEvaluator>
};

}   // namespace Kernel
