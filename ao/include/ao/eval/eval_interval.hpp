#pragma once

#include <Eigen/Eigen>

#include "ao/eval/base.hpp"
#include "ao/eval/interval.hpp"

namespace Kernel {

class IntervalEvaluator : public BaseEvaluator
{
public:
    IntervalEvaluator(std::shared_ptr<Tape> t);
    IntervalEvaluator(std::shared_ptr<Tape> t,
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
    /*  i[clause] is the interval result for that clause */
    std::vector<Interval::I> i;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void evalClause(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);
};

}   // namespace Kernel
