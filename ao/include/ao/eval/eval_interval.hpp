#pragma once

#include <Eigen/Eigen>

#include "ao/eval/interval.hpp"
#include "ao/eval/tape.hpp"

namespace Kernel {

class IntervalEvaluator
{
public:
    IntervalEvaluator(Tape& t);
    IntervalEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

    /*
     *  Interval evaluation
     */
    Interval::I eval(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper);
    Interval::I evalAndPush(const Eigen::Vector3f& lower,
                            const Eigen::Vector3f& upper);

    /*
     *  Pops the tape
     *  (must be paired against evalAndPush)
     */
    void pop() { tape.pop(); }

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value);

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void evalClause(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

protected:
    Tape& tape;

    /*  i[clause] is the interval result for that clause */
    std::vector<Interval::I> i;
};

}   // namespace Kernel
