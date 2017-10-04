#pragma once

#include <Eigen/Eigen>

#include "ao/eval/interval.hpp"
#include "ao/eval/tape.hpp"

namespace Kernel {

class PointEvaluator
{
public:
    PointEvaluator(Tape& t);
    PointEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

    /*
     *  Single-point evaluation
     */
    float eval(const Eigen::Vector3f& pt);
    float evalAndPush(const Eigen::Vector3f& pt);

    /*
     *  Evaluates the given point using whichever tape in the tape stack
     *  contains the point in its region (this is useful when we're not
     *  sure about which region the points fits into)
     */
    float baseEval(const Eigen::Vector3f& p);

protected:
    Tape& tape;

    /*  f(clause) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, 1> f;

public:
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

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel

