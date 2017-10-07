#pragma once

#include <Eigen/Eigen>

#include "ao/eval/eval_point.hpp"

namespace Kernel {

class DerivEvaluator : public PointEvaluator
{
public:
    DerivEvaluator(std::shared_ptr<Tape> t);
    DerivEvaluator(std::shared_ptr<Tape> t,
                   const std::map<Tree::Id, float>& vars);

protected:
    /*  d(axis, clause) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<float, 3, Eigen::Dynamic> d;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);
public:
    /*
     *  Single-point evaluation (return dx, dy, dz, distance)
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<DerivEvaluator>
};

}   // namespace Kernel



