#pragma once
#include <map>

#include "glm/vec3.hpp"

#include "kernel/tree/cache.hpp"
#include "kernel/eval/evaluator.hpp"

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
            const glm::vec3 pos={0,0,0}, const Mask& mask=Mask(),
            unsigned gas=25000);
    std::pair<float, Solution> findRoot(
            Evaluator& t, const std::map<Tree::Id, float>& vars,
            const glm::vec3 pos={0,0,0}, const Mask& mask=Mask(),
            unsigned gas=25000);

}   // namespace Solver
}   // namespace Kernel
