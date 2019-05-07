/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/eval_point.hpp"

namespace Kernel {

class DerivEvaluator : public PointEvaluator
{
public:
    DerivEvaluator(const Tree& root);
    DerivEvaluator(const Tree& root,
                   const std::map<Tree::Id, float>& vars);
    DerivEvaluator(std::shared_ptr<Deck> t);
    DerivEvaluator(std::shared_ptr<Deck> t,
                   const std::map<Tree::Id, float>& vars);

protected:
    /*  d(axis, clause) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<float, 3, Eigen::Dynamic> d;

    /*  Per-clause evaluation, used in tape walking */
    void operator()(std::vector<Token>::const_reverse_iterator& itr);
public:
    /*
     *  Single-point evaluation (return dx, dy, dz, distance)
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt,
                          std::shared_ptr<Tape> tape);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<DerivEvaluator>
};

}   // namespace Kernel



