#include "ao/eval/result.hpp"

namespace Kernel {

void Result::resize(Index clauses, Index vars)
{
#ifdef __AVX__
    mf.resize(clauses);
    mdx.resize(clauses);
    mdy.resize(clauses);
    mdz.resize(clauses);

    f  = reinterpret_cast<float(*)[N]>(&mf[0]);
    dx = reinterpret_cast<float(*)[N]>(&mdx[0]);
    dy = reinterpret_cast<float(*)[N]>(&mdy[0]);
    dz = reinterpret_cast<float(*)[N]>(&mdz[0]);
#else
    f.resize(clauses);
    dx.resize(clauses);
    dy.resize(clauses);
    dz.resize(clauses);
#endif
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
        f[clause][i] = v;
        dx[clause][i] = 0;
        dy[clause][i] = 0;
        dz[clause][i] = 0;
    }

    i[clause] = Interval(v, v);
}

void Result::setGradient(Index clause, Index var)
{
    // Drop a 1 in the Jacobian row at the var index
    j[clause][var] = 1;
}

void Result::setDeriv(float x, float y, float z, Index clause)
{
    for (size_t i=0; i < N; ++i)
    {
        dx[clause][i] = x;
        dy[clause][i] = y;
        dz[clause][i] = z;
    }
}

}   // namespace Kernel
