#pragma once

#include <Eigen/Eigen>

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_deriv.hpp"

namespace Kernel {

class JacobianEvaluator : public DerivEvaluator
{
public:
    JacobianEvaluator(std::shared_ptr<Tape> t);
    JacobianEvaluator(std::shared_ptr<Tape> t,
                      const std::map<Tree::Id, float>& vars);

    /*
     *  Returns the gradient with respect to all VAR nodes
     */
    std::map<Tree::Id, float> gradient(const Eigen::Vector3f& p);

protected:
    /*
     *  Raw clause evaluation is done here!
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*  j(clause, var) = dclause / dvar */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> j;

    friend class Tape; // for rwalk<JacobianEvaluator>
};

}   // namespace Kernel
