/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/eval_feature.hpp"

namespace libfive {

class JacobianEvaluator : public FeatureEvaluator
{
public:
    JacobianEvaluator(const Tree& root);
    JacobianEvaluator(const Tree& root,
                      const std::map<Tree::Id, float>& vars);
    JacobianEvaluator(std::shared_ptr<Deck> t);
    JacobianEvaluator(std::shared_ptr<Deck> t,
                      const std::map<Tree::Id, float>& vars);

    /*
     *  Returns the gradient with respect to all VAR nodes
     */
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p);
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p,
                                       const Tape& tape);

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

protected:
    /*  j(var) = dout / dvar */
    Eigen::Array<float, Eigen::Dynamic, 1> j;
};

}   // namespace libfive
