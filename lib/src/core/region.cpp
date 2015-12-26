#include <algorithm>

#include "ao/core/region.hpp"

Region::Region(Interval x, Interval y, Interval z, double res)
    : Region(x, y, z, res, res, res)
{
    // Nothing to do here
}

Region::Region(Interval x, Interval y, Interval z,
               double rx, double ry, double rz)
    : X(x, rx), Y(y, ry), Z(z, rz)
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

Region::DiscreteRange::DiscreteRange(Interval i, double res)
    : interval(i), min(0),
      size(std::max((size_t)1, (size_t)(res * (i.upper() - i.lower())))),
      ptr(new double[size]), root(true)
{
    for (unsigned index=0; index < size; ++index)
    {
        const double frac = (index + 0.5) / size;
        ptr[index] = lower() * (1 - frac) + upper() * frac;
    }
}
Region::DiscreteRange::DiscreteRange(const DiscreteRange& other)
    : interval(other.interval), min(other.min), size(other.size),
      ptr(other.ptr), root(false)
{
    // Nothing to do here
}

Region::DiscreteRange::DiscreteRange(Interval i, size_t min, size_t size,
                                     double* ptr)
    : interval(i), min(min), size(size), ptr(ptr), root(false)
{
    // Nothing to do here
}

Region::DiscreteRange::~DiscreteRange()
{
    if (root)
    {
        delete [] ptr;
    }
}
////////////////////////////////////////////////////////////////////////////////

std::pair<Region::DiscreteRange, Region::DiscreteRange>
Region::DiscreteRange::split() const
{
    const size_t half = size / 2;
    const double frac = half / double(size);

    const double middle = upper() * frac + lower() * (1 - frac);

    return {DiscreteRange(Interval(lower(), middle), min, half, ptr),
            DiscreteRange(Interval(middle, upper()), min + half,
                          size - half, ptr + half)};
}
