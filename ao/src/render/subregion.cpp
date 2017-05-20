#include <array>

#include "ao/render/subregion.hpp"
#include "ao/render/region.hpp"
#include "ao/render/octree.hpp"

namespace Kernel {

Subregion::Subregion(const Region& r)
    : X(r.X.bounds, r.X.values),
      Y(r.Y.bounds, r.Y.values),
      Z(r.Z.bounds, r.Z.values)
{
    // Nothing to do here
}

Subregion::Subregion(const Axis& x, const Axis& y, const Axis& z)
    : X(x), Y(y), Z(z)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

std::pair<Subregion, Subregion> Subregion::split() const
{
    if (Z.size > Y.size && Z.size > X.size)
    {
        auto zs = Z.split();
        return std::make_pair(Subregion(X, Y, zs.first), Subregion(X, Y, zs.second));
    }
    else if (Y.size > X.size)
    {
        auto ys = Y.split();
        return std::make_pair(Subregion(X, ys.first, Z), Subregion(X, ys.second, Z));
    }
    else
    {
        auto xs = X.split();
        return std::make_pair(Subregion(xs.first, Y, Z), Subregion(xs.second, Y, Z));
    }
}

std::pair<Subregion, Subregion> Subregion::splitXY() const
{
    if (Y.size > X.size)
    {
        auto ys = Y.split();
        return std::make_pair(Subregion(X, ys.first, Z), Subregion(X, ys.second, Z));
    }
    else
    {
        auto xs = X.split();
        return std::make_pair(Subregion(xs.first, Y, Z), Subregion(xs.second, Y, Z));
    }
}

bool Subregion::canSplit() const
{
    return X.size > 1 || Y.size > 1 || Z.size > 1;
}

bool Subregion::canSplitXY() const
{
    return X.size > 1 || Y.size > 1;
}

size_t Subregion::voxels() const
{
    return X.size * Y.size * Z.size;
}

////////////////////////////////////////////////////////////////////////////////

Subregion::Axis::Axis(Interval i, const std::vector<float>& vs)
    : Axis(i, &vs[0], vs.size(), 0)
{
    // Nothing to do here
}

Subregion::Axis::Axis(Interval i, const float* ptr,
                      size_t size, size_t min)
    : bounds(i), ptr(ptr), min(min), size(size)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

std::pair<Subregion::Axis, Subregion::Axis> Subregion::Axis::split() const
{
    const size_t half = size / 2;
    const float frac = half / float(size);

    const float middle = upper() * frac + lower() * (1 - frac);

    return {Axis(Interval(lower(), middle), ptr, half, min),
            Axis(Interval(middle, upper()), ptr + half,
                 size - half, min + half)};
}

}   // namespace Kernel
