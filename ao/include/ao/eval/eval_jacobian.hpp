#pragma once

#include <Eigen/Eigen>

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_point.hpp"

namespace Kernel {

class JacobianEvaluator : public PointEvaluator
{
public:
    JacobianEvaluator(Tape& t);
    JacobianEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

    /*
     *  Returns the gradient with respect to all VAR nodes
     */
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p);

protected:
    /*
     *  Raw clause evaluation is done here!
     */
    void evalClause(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*  j(clause, var) = dclause / dvar */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> j;
};

}   // namespace Kernel
