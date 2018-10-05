/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <Eigen/Eigen>
#include <array>

namespace Kernel {

template <unsigned int N>
class Region
{
public:
    typedef Eigen::Array<double, N, 1> Pt;
    typedef Eigen::Array<double, 3 - N, 1> Perp;

    /*
     *  Check if the given point is in the region
     */
    bool contains(Pt p, double epsilon = 1e-8) const
    {
        return (p >= (lower - epsilon)).all() && (p <= (upper + epsilon)).all();
    }

    /*
     *  Helper function to get center of region
     */
    Pt center() const { return (lower + upper) / 2; }

    /*
     *  Constructs a region with the given bounds
     */
    Region(Pt lower, Pt upper) : lower(lower), upper(upper),
                                 perp(Perp::Zero()) {}

    /*
     *  Construct a region with the given bounds
     *  and perpendicular coordinate(s)
     */
    Region(Pt lower, Pt upper, Perp p) : lower(lower), upper(upper), perp(p) {}

    /*
     *  Default constructor for an empty region
     */
    Region() : lower(Pt::Zero()), upper(Pt::Zero()) {}

    Pt& operator[](std::size_t idx)
    {
        switch(idx)
        {
            case 0: return lower;
            case 1: return upper;
            default: assert(false);
        }
        assert(false);
        return lower;
    }

    /*
     *  Splits along all axes
     */
    std::array<Region, 1 << N> subdivide() const
    {
        // Default-construct empty regions
        std::array<Region, 1 << N> out = {};
        auto c = center();

        for (unsigned i=0; i < (1 << N); ++i)
        {
            auto a = Eigen::Array<double, N, 1>(0);
            for (unsigned j=0; j < N; ++j)
            {
                a(j) = (i & (1 << j)) > 0;
            }
            out[i] = Region(a.select(c, lower), a.select(upper, c), perp);
        }
        return out;
    }

    /*
     *  Returns the volume of the region in arbitrary units
     */
    double volume() const
    {
        double out = 1;
        for (unsigned i=0; i < N; ++i)
        {
            out *= upper(i) - lower(i);
        }
        return out;
    }

    bool empty() const
    {
        return (lower.array() == 0).all() &&
               (upper.array() == 0).all();
    }

    Eigen::Array3d lower3() const
    {
        Eigen::Array3d out;
        out << lower, perp;
        return out;
    }

    Eigen::Array3d upper3() const
    {
        Eigen::Array3d out;
        out << upper, perp;
        return out;
    }

    Eigen::Vector3d corner3(unsigned i) const
    {
        Eigen::Vector3d out;
        out << corner(i), perp;
        return out;
    }

    Eigen::Vector3f corner3f(unsigned i) const
    {
        return corner3(i).template cast<float>();
    }

    Eigen::Matrix<double, N, 1> corner(unsigned i) const
    {
        Eigen::Matrix<double, N, 1> out;
        for (unsigned axis=0; axis < N; ++axis)
        {
            out(axis) = (i & (1 << axis)) ? upper(axis)
                                          : lower(axis);
        }
        return out;
    }

    /*  Lower and upper bounds for the region  */
    Pt lower, upper;

    /*  perp is the coordinates on perpendicular axes, used when converting
     *  a 2D region into 3D coordinates for Interval evaluation  */
    Perp perp;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel
