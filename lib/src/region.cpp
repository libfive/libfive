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
    if (X.size > Y.size && X.size > Z.size)
    {
        auto xs = X.split();
        return std::make_pair(Region(xs.first, Y, Z), Region(xs.second, Y, Z));
    }
    else if (Y.size > Z.size)
    {
        auto ys = Y.split();
        return std::make_pair(Region(X, ys.first, Z), Region(X, ys.second, Z));
    }
    else
    {
        auto zs = Z.split();
        return std::make_pair(Region(X, Y, zs.first), Region(X, Y, zs.second));
    }
}

bool Region::canSplit() const
{
    return X.size > 1 || Y.size > 1 || Z.size > 1;
}

////////////////////////////////////////////////////////////////////////////////

Region::DiscreteRange::DiscreteRange(Interval i, double res)
    : Interval(i), min(0), size(res * (i.upper() - i.lower()))
{
    // Nothing to do here
}

Region::DiscreteRange::DiscreteRange(Interval i, size_t min, size_t size)
    : Interval(i), min(min), size(size)
{
    // Nothing to do here
}

std::pair<Region::DiscreteRange, Region::DiscreteRange>
Region::DiscreteRange::split() const
{
    const size_t mid = size / 2;
    const double delta = upper() - lower();

    return {DiscreteRange(Interval(lower(), delta * mid / size),
                          min, mid),
            DiscreteRange(Interval(delta * mid / size, upper()),
                          min + mid, size - mid)};
}
