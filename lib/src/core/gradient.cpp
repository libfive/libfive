#include <algorithm>
#include <cmath>

#include "ao/core/gradient.hpp"

Gradient operator+(const Gradient& a, const Gradient& b)
{
    return Gradient(a.v + b.v, a.dx + b.dx, a.dy + b.dy, a.dz + b.dz);
}

Gradient operator-(const Gradient& a, const Gradient& b)
{
    return Gradient(a.v - b.v, a.dx - b.dx, a.dy - b.dy, a.dz - b.dz);
}

Gradient operator*(const Gradient& a, const Gradient& b)
{
    // Product rule
    return Gradient(a.v * b.v,
            a.v * b.dx + b.v * a.dx,
            a.v * b.dy + b.v * a.dy,
            a.v * b.dz + b.v * a.dz);
}

Gradient operator/(const Gradient& a, const Gradient& b)
{
    // Quotient rule
    const double p = pow(b.v, 2);
    return Gradient(a.v / b.v,
            (b.v * a.dx - a.v * b.dx) / p,
            (b.v * a.dy - a.v * b.dy) / p,
            (b.v * a.dz - a.v * b.dz) / p);
}


Gradient _min(const Gradient& a, const Gradient& b)
{
    return (a.v < b.v) ? a : b;
}

Gradient _max(const Gradient& a, const Gradient& b)
{
    return (a.v < b.v) ? b : a;
}

Gradient _cond_nz(const Gradient& cond, const Gradient& a, const Gradient& b)
{
    return (cond.v < 0) ? a : b;
}

Gradient pow(const Gradient& a, const Gradient& b)
{
    const double p = pow(a.v, b.v - 1);
    const double m = a.v * log(a.v);

    // If a.v is negative, then m will be NaN (because of log's domain).
    // We work around this by checking if d/d{xyz}(B) == 0 and using a
    // simplified expression if that's true.
    return Gradient(pow(a.v, b.v),
        p * (b.v*a.dx + (b.dx ? m*b.dx : 0)),
        p * (b.v*a.dy + (b.dy ? m*b.dy : 0)),
        p * (b.v*a.dz + (b.dz ? m*b.dz : 0)));
}

Gradient sqrt(const Gradient& a)
{
    if (a.v < 0)
    {
        return Gradient(0);
    }
    else
    {
        const double v = sqrt(a.v);
        return Gradient(v, a.dx / (2 * v),
                           a.dy / (2 * v),
                           a.dz / (2 * v));
    }
}

Gradient operator-(const Gradient& a)
{
    return Gradient(-a.v, -a.dx, -a.dy, -a.dz);
}
