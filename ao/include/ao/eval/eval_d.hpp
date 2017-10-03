#pragma once

#include <Eigen/Eigen>

#include "ao/eval/eval_a.hpp"

namespace Kernel {

class DerivArrayEvaluator : public ArrayEvaluator
{
public:
    DerivArrayEvaluator(Tape& t);
    DerivArrayEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

protected:
    typedef Eigen::Array<float, 4, N> ResultArray;

    /*  d(clause).col(index) is a set of partial derivatives [dx, dy, dz, w] */
    Eigen::Array<Eigen::Array<float, 4, N>, Eigen::Dynamic, 1> d;

public:
    /*
     *  Single-point evaluation (return dx, dy, dz, distance)
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);

    /*
     *  Multi-point evaluation (values must be stored with set)
     */
    Eigen::Block<ResultArray, 4, Eigen::Dynamic> derivs(size_t count);

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


