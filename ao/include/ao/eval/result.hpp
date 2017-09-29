#pragma once

#include <array>
#include <vector>

#include <Eigen/Eigen>

#include "ao/eval/interval.hpp"
#include "ao/eval/clause.hpp"

namespace Kernel {


struct Result {
    /*
     *  Constructs a result object with appropriate array sizes
     */
    Result(size_t clauses, size_t vars=0);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     *  Gradient is set to 0
     */
    void fill(float v, Clause::Id clause);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     */
    void setValue(float v, Clause::Id clause);

    /*
     *  Marks that j(clause, var) = 1
     */
    void setGradient(Clause::Id clause, size_t var);

    /*
     *  Fills the derivative arrays with the given values
     */
    void setDeriv(Eigen::Vector3f d, Clause::Id clause);

    // This is the number of samples that we can process in one pass
    static constexpr size_t N = 256;

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  f(clause, index) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, N, Eigen::RowMajor> f;

    /*  d(clause).col(index) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<Eigen::Array<float, 3, N>, Eigen::Dynamic, 1> d;

    /*  j(clause, var) = dclause / dvar */
    Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> j;

    /*  i[clause] is the interval result for that clause */
    std::vector<Interval::I> i;

    /*  ambig(index) returns whether a particular slot is ambiguous */
    Eigen::Array<bool, N, 1> ambig;
};

}   // namespace Kernel
