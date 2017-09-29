#include "ao/eval/result.hpp"

namespace Kernel {

constexpr size_t Result::N;

Result::Result(size_t clauses, size_t vars)
    : f(clauses, N), d(clauses, 1), j(clauses, vars)
{
    i.resize(clauses);
    j = 0;
}

void Result::fill(float v, Clause::Id clause)
{
    // Load a constant into the value row
    setValue(v, clause);

    // Fill the Gradient row with zeros
    j.row(clause) = 0;
}

void Result::setValue(float v, Clause::Id clause)
{
    for (unsigned i=0; i < N; ++i)
    {
        f(clause, i) = v;
        d(clause).col(i) = 0;
    }

    i[clause] = Interval::I(v, v);
}

void Result::setGradient(Clause::Id clause, size_t var)
{
    // Drop a 1 in the Jacobian row at the var index
    j(clause, var) = 1;
}

void Result::setDeriv(Eigen::Vector3f deriv, Clause::Id clause)
{
    for (size_t i=0; i < N; ++i)
    {
        d(clause).col(i) = deriv;
    }
}

}   // namespace Kernel
