#include "ao/eval/result.hpp"

namespace Kernel {

constexpr Result::Index Result::N;

Result::Result(Index clauses, Index vars)
    : f(clauses, N), dx(clauses, N), dy(clauses, N), dz(clauses, N)
{
    i.resize(clauses);
    j.resize(clauses);
    for (auto& d : j)
    {
        d.resize(vars);
    }
}

void Result::fill(float v, Index clause)
{
    setValue(v, clause);

    // Fill the Gradient row with zeros
    for (auto& d : j[clause])
    {
        d = 0;
    }
}

void Result::setValue(float v, Index clause)
{
    for (unsigned i=0; i < N; ++i)
    {
        f(clause, i) = v;
        dx(clause, i) = 0;
        dy(clause, i) = 0;
        dz(clause, i) = 0;
    }

    i[clause] = Interval::I(v, v);
}

void Result::setGradient(Index clause, Index var)
{
    // Drop a 1 in the Jacobian row at the var index
    j[clause][var] = 1;
}

void Result::setDeriv(Eigen::Vector3f d, Index clause)
{
    for (size_t i=0; i < N; ++i)
    {
        dx(clause, i) = d.x();
        dy(clause, i) = d.y();
        dz(clause, i) = d.z();
    }
}

}   // namespace Kernel
