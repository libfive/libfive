/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/base.hpp"
#include "libfive/eval/clause.hpp"

namespace Kernel {
class Tape; /* Forward declaration */

class PointEvaluator : public BaseEvaluator
{
public:
    PointEvaluator(std::shared_ptr<Deck> t);
    PointEvaluator(std::shared_ptr<Deck> t,
                   const std::map<Tree::Id, double>& vars);

    /*
     *  Single-point evaluation
     */
    double eval(const Eigen::Vector3d& pt);
    double eval(const Eigen::Vector3d& pt, std::shared_ptr<Tape> tape);

    std::pair<double, std::shared_ptr<Tape>> evalAndPush(
            const Eigen::Vector3d& pt);
    std::pair<double, std::shared_ptr<Tape>> evalAndPush(
            const Eigen::Vector3d& pt,
            std::shared_ptr<Tape> tape);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, double value);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

protected:
    /*  f(clause) is a specific data point */
    Eigen::Array<double, Eigen::Dynamic, 1> f;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    friend class Tape; // for rwalk<PointEvaluator>
};

}   // namespace Kernel

