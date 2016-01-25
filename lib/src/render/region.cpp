#include <algorithm>

#include "ao/render/region.hpp"

Region::Region(Interval x, Interval y, Interval z, float res)
    : Region(x, y, z, res, res, res)
{
    // Nothing to do here
}

Region::Region(Interval x, Interval y, Interval z,
               float rx, float ry, float rz)
    : X(x, rx), Y(y, ry), Z(z, rz)
{
    // Nothing to do here
}

Subregion Region::view() const
{
    return Subregion(*this);
}

////////////////////////////////////////////////////////////////////////////////

Region::Axis::Axis(Interval i, float res)
    : bounds(i)
{
    size_t size = std::max((size_t)1,
                           (size_t)(res * (i.upper() - i.lower())));
    for (unsigned index=0; index < size; ++index)
    {
        const float frac = (index + 0.5) / size;
        values.push_back(i.lower() * (1 - frac) + i.upper() * frac);
    }
}
