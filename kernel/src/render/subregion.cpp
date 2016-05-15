/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <array>

#include "ao/kernel/render/subregion.hpp"
#include "ao/kernel/render/region.hpp"
#include "ao/kernel/render/octree.hpp"

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

std::array<Subregion, 8> Subregion::octsect() const
{
    assert(X.size == Y.size && X.size == Z.size);
    assert(X.size >= 2 && X.size % 2 == 0);
    assert(Y.size >= 2 && Y.size % 2 == 0);
    assert(Z.size >= 2 && Z.size % 2 == 0);

    auto x = split();

    auto xy = x.first.split();
    auto Xy = x.second.split();

    auto xyz = xy.first.split();
    auto xYz = xy.second.split();
    auto Xyz = Xy.first.split();
    auto XYz = Xy.second.split();

    // Order subregions based on Octree axes
    std::array<Subregion*, 8> out;
    out[0] = &xyz.first;
    out[Octree::AXIS_X] = &Xyz.first;
    out[Octree::AXIS_Y] = &xYz.first;
    out[Octree::AXIS_X|Octree::AXIS_Y] = &XYz.first;
    out[Octree::AXIS_Z] = &xyz.second;
    out[Octree::AXIS_X|Octree::AXIS_Z] = &Xyz.second;
    out[Octree::AXIS_Y|Octree::AXIS_Z] = &xYz.second;
    out[Octree::AXIS_X|Octree::AXIS_Y|Octree::AXIS_Z] = &XYz.second;

    return {{*out[0], *out[1], *out[2], *out[3],
             *out[4], *out[5], *out[6], *out[7]}};
}

bool Subregion::canOctsect() const
{
    return X.size == Y.size && X.size == Z.size &&
           X.size >= 2 && X.size % 2 == 0 &&
           Y.size >= 2 && Y.size % 2 == 0 &&
           Z.size >= 2 && Z.size % 2 == 0;
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
