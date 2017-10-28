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
#include <map>
#include <set>

#include <Eigen/Eigen>

#include "ao/tree/cache.hpp"
#include "ao/eval/eval_jacobian.hpp"

namespace Kernel {

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
            JacobianEvaluator& e, std::map<Tree::Id, float> vars,
            const Eigen::Vector3f pos={0,0,0}, const Mask& mask=Mask(),
            unsigned gas=25000);

}   // namespace Solver
}   // namespace Kernel
