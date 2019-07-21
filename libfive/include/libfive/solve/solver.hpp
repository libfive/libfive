/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <map>
#include <set>

#include <Eigen/Eigen>

#include "libfive/tree/cache.hpp"
#include "libfive/eval/eval_jacobian.hpp"

namespace libfive {

class Tree;

namespace Solver
{
    typedef std::map<Tree::Id, float> Solution;
    typedef std::set<Tree::Id> Mask;

    /*
     *  Finds a set of variables that drive t to zero
     *
     *  pos is the x,y,z coordinates at which to solve
     *  Initial conditions are the variable values in vars
     */
    std::pair<float, Solution> findRoot(
            const Tree& t, const std::map<Tree::Id, float>& vars,
            const Eigen::Vector3f pos={0,0,0}, const Mask& mask=Mask(),
            unsigned gas=25000);
    std::pair<float, Solution> findRoot(
            JacobianEvaluator& e, const std::shared_ptr<Tape>& tape,
            std::map<Tree::Id, float> vars,
            const Eigen::Vector3f pos={0,0,0}, const Mask& mask=Mask(),
            unsigned gas=25000);

}   // namespace Solver
}   // namespace libfive
