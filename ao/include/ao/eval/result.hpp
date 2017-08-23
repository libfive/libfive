#pragma once

#include <array>
#include <vector>

#include <Eigen/Eigen>

#include "ao/eval/interval.hpp"
#include "ao/eval/clause.hpp"

namespace Kernel {


struct Result {
    typedef Clause::Id Index;

    /*
     *  Constructs a result object with appropriate array sizes
     */
    Result(Index clauses, Index vars=0);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     *  Gradient is set to 0
     */
    void fill(float v, Index clause);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     */
    void setValue(float v, Index clause);

    /*
     *  Marks that j[clause][var] = 1
     */
    void setGradient(Index clause, Index var);

    /*
     *  Fills the derivative arrays with the given values
     */
    void setDeriv(Eigen::Vector3f d, Index clause);

    // This is the number of samples that we can process in one pass
    static constexpr Index N = 256;

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  f(clause, index) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, N, Eigen::RowMajor> f;

    /*  d(clause).col(index) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<Eigen::Array<float, 3, N>, Eigen::Dynamic, 1> d;

    /*  j(clause, var) = dclause / dvar */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> j;

    std::vector<Interval::I> i;
};

}   // namespace Kernel
