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

std::tuple<std::vector<double>,
           std::vector<double>,
           std::vector<double>> Region::flatten() const
{
    std::vector<double> x, y, z;

    {
        const size_t voxels = X.size * Y.size * Z.size;
        x.reserve(voxels);
        y.reserve(voxels);
        z.reserve(voxels);
    }

    const auto xs = X.flatten();
    const auto ys = Y.flatten();
    const auto zs = Z.flatten();

    forEach([&](size_t i, size_t j, size_t k)
            {
                x.push_back(xs[i]);
                y.push_back(ys[j]);
                z.push_back(zs[k]);
            });

    return std::make_tuple(x, y, z);
}

void Region::forEach(std::function<void(size_t, size_t, size_t)> f) const
{
    for (unsigned k=0; k <= Z.size; ++k)
    {
        for (unsigned j=0; j <= Y.size; ++j)
        {
            for (unsigned i=0; i <= X.size; ++i)
            {
                f(i, j, k);
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

Region::DiscreteRange::DiscreteRange(Interval i, double res)
    : interval(i), min(0), size(res * (i.upper() - i.lower()))
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
    const size_t mid = size / 2;
    const double delta = upper() - lower();

    return {DiscreteRange(Interval(lower(), delta * mid / size),
                          min, mid),
            DiscreteRange(Interval(delta * mid / size, upper()),
                          min + mid, size - mid)};
}

std::vector<double> Region::DiscreteRange::flatten() const
{
    std::vector<double> out;
    out.reserve(size + 1);

    for (unsigned i=0; i <= size; ++i)
    {
        out.push_back(lower() + (upper() - lower()) * i / size);
    }
    return out;
}
