#pragma once

#include <Eigen/Eigen>

#include "ao/eval/interval.hpp"
#include "ao/eval/tape.hpp"

namespace Kernel {

class ArrayEvaluator
{
public:
    ArrayEvaluator(Tape& t);
    ArrayEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

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

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3f& p, size_t index)
    {
        f(tape.X, index) = p.x();
        f(tape.Y, index) = p.y();
        f(tape.Z, index) = p.z();
    }

    /*
     *  Multi-point evaluation (values must be stored with set)
     */
    const float* values(size_t count);

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

    /*
     *  Used when pushing tape
     */
    Tape::Keep check(Opcode::Opcode op, Clause::Id a, Clause::Id b) const;
    void getBounds(Interval::I& X, Interval::I& Y, Interval::I& Z) const;
    static Tape::Type TapeType;

    /*  This is the number of samples that we can process in one pass */
    static constexpr size_t N=256;

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

protected:
    Tape& tape;

    /*  Stored in values() and used in evalClause() to decide how much of the
     *  array we're addressing at once  */
    size_t count;

    /*  f(clause, index) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, N, Eigen::RowMajor> f;
};

}   // namespace Kernel

