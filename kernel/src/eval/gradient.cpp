#include <algorithm>
#include <cmath>

#include "ao/kernel/eval/gradient.hpp"

Gradient operator+(const Gradient& a, const Gradient& b)
{
    return Gradient(a.v + b.v, a.d + b.d);
}

Gradient operator-(const Gradient& a, const Gradient& b)
{
    return Gradient(a.v - b.v, a.d - b.d);
}

Gradient operator*(const Gradient& a, const Gradient& b)
{
    // Product rule
    return Gradient(a.v * b.v, a.v * b.d + b.v * a.d);
}

Gradient operator/(const Gradient& a, const Gradient& b)
{
    // Quotient rule
    const float p = pow(b.v, 2);
    return Gradient(a.v / b.v, (b.v * a.d - a.v * b.d) / p);
}

Gradient _min(const Gradient& a, const Gradient& b)
{
    return (a.v < b.v) ? a : b;
}

Gradient _max(const Gradient& a, const Gradient& b)
{
    return (a.v < b.v) ? b : a;
}

Gradient pow(const Gradient& a, const Gradient& b)
{
    const float p = pow(a.v, b.v - 1);
    const float m = a.v * log(a.v);

    // If a.v is negative, then m will be NaN (because of log's domain).
    // We work around this by checking if d/d{xyz}(B) == 0 and using a
    // simplified expression if that's true.
    return Gradient(pow(a.v, b.v),
        {p * (b.v*a.d.x + (b.d.x ? m*b.d.x : 0)),
         p * (b.v*a.d.y + (b.d.y ? m*b.d.y : 0)),
         p * (b.v*a.d.z + (b.d.z ? m*b.d.z : 0))});
}

Gradient sqrt(const Gradient& a)
{
    if (a.v < 0)
    {
        return Gradient(0);
    }
    else
    {
        const float v = sqrt(a.v);
        return Gradient(v, a.d / (2 * v));
    }
}

Gradient _abs(const Gradient& a)
{
    if (a.v < 0)
    {
        return Gradient(-a.v, -a.d);
    }
    else
    {
        return a;
    }
}

Gradient operator-(const Gradient& a)
{
    return Gradient(-a.v, -a.d);
}
