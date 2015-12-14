#include <algorithm>

#include "region.hpp"

Region::Region(Interval x, Interval y, Interval z, double res)
    : X(x, res), Y(y, res), Z(z, res)
{
    // Nothing to do here
}

Region::Region(DiscreteRange x, DiscreteRange y, DiscreteRange z)
    : X(x), Y(y), Z(z)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

std::pair<Region, Region> Region::split() const
{
    if (Z.size > Y.size && Z.size > X.size)
    {
        auto zs = Z.split();
        return std::make_pair(Region(X, Y, zs.first), Region(X, Y, zs.second));
    }
    else if (Y.size > X.size)
    {
        auto ys = Y.split();
        return std::make_pair(Region(X, ys.first, Z), Region(X, ys.second, Z));
    }
    else
    {
        auto xs = X.split();
        return std::make_pair(Region(xs.first, Y, Z), Region(xs.second, Y, Z));
    }
}

bool Region::canSplit() const
{
    return X.size > 1 || Y.size > 1 || Z.size > 1;
}

size_t Region::voxels() const
{
    return X.size * Y.size * Z.size;
}

////////////////////////////////////////////////////////////////////////////////

void Region::forEach(std::function<void(size_t, size_t, size_t)> f) const
{
    for (unsigned k=0; k < Z.size; ++k)
    {
        for (unsigned j=0; j < Y.size; ++j)
        {
            for (unsigned i=0; i < X.size; ++i)
            {
                f(i, j, k);
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

Region::DiscreteRange::DiscreteRange(Interval i, double res)
    : interval(i), min(0),
      size(std::max((size_t)1, (size_t)(res * (i.upper() - i.lower()))))
{
    // Nothing to do here
}

Region::DiscreteRange::DiscreteRange(Interval i, size_t min, size_t size)
    : interval(i), min(min), size(size)
{
    // Nothing to do here
}

std::pair<Region::DiscreteRange, Region::DiscreteRange>
Region::DiscreteRange::split() const
{
    const size_t half = size / 2;
    const double frac = half / double(size);

    const double middle = upper() * frac + lower() * (1 - frac);

    return {DiscreteRange(Interval(lower(), middle), min, half),
            DiscreteRange(Interval(middle, upper()), min + half, size - half)};
}

double Region::DiscreteRange::pos(size_t i) const
{
    if (size == 0)
    {
        return lower();
    }
    else
    {
        const double frac = (i + 0.5) / size;
        return lower() * (1 - frac) + upper() * frac;
    }
}
