#include "ao/eval/result.hpp"

namespace Kernel {

constexpr Result::Index Result::N;

Result::Result(Index clauses, Index vars)
    : f(clauses, N), d(clauses, N), j(clauses, vars)
{
    i.resize(clauses);
    j = 0;
}

void Result::fill(float v, Index clause)
{
    // Load a constant into the value row
    setValue(v, clause);

    // Fill the Gradient row with zeros
    j.row(clause) = 0;
}

void Result::setValue(float v, Index clause)
{
    for (unsigned i=0; i < N; ++i)
    {
        f(clause, i) = v;
        d(clause, i) = 0;
    }

    i[clause] = Interval::I(v, v);
}

void Result::setGradient(Index clause, Index var)
{
    // Drop a 1 in the Jacobian row at the var index
    j(clause, var) = 1;
}

void Result::setDeriv(Eigen::Vector3f deriv, Index clause)
{
    for (size_t i=0; i < N; ++i)
    {
        d(clause, i) = deriv;
    }
}

}   // namespace Kernel
