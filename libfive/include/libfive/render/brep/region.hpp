/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>
#include <array>

#include "libfive/render/brep/util.hpp"

namespace libfive {

template <unsigned int N>
class Region
{
public:
    typedef Eigen::Array<double, N, 1> Pt;
    typedef Eigen::Array<double, 3 - N, 1> Perp;

    /*
     *  Check if the given point is in the region
     *  As epsilon gets larger, the test becomes more inclusive
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
    Region(Pt lower, Pt upper)
        : lower(lower), upper(upper), perp(Perp::Zero()), level(-1)
    { /* Nothing to do here */ }

    /*
     *  Construct a region with the given bounds
     *  and perpendicular coordinate(s)
     */
    Region(Pt lower, Pt upper, Perp p, int32_t level=-1)
        : lower(lower), upper(upper), perp(p), level(level)
    { /* Nothing to do here */ }

    /*
     *  Default constructor for an empty region
     */
    Region()
        : lower(Pt::Zero()), upper(Pt::Zero()), level(-1)
    { /* Nothing to do here */ }

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

    const Pt& operator[](std::size_t idx) const
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
        // If the region has a level, then it must be non-zero
        //
        // Subdividing a no-level region is acceptable, because the
        // level is an optional field, but if it is present, then it
        // must be non-zero.
        assert(level > 0 || level == -1);

        // Default-construct empty regions
        std::array<Region, 1 << N> out = {};
        auto c = center();
        const auto new_level = (level == -1) ? -1 : (level - 1);

        for (unsigned i=0; i < (1 << N); ++i)
        {
            auto a = Eigen::Array<double, N, 1>(0);
            for (unsigned j=0; j < N; ++j)
            {
                a(j) = (i & (1 << j)) > 0;
            }
            out[i] = Region(a.select(c, lower), a.select(upper, c),
                            perp, new_level);
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

    Region<3> region3() const {
        return Region<3>(lower3(), upper3());
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

    /*
     *  Returns a region with only the masked axes present.
     *
     *  This is useful to reduce a 3D region into a region containing
     *  a particular 2D space.  Axes are dropped in order, e.g. masking
     *  X and Z from a 3D region would produce a region with [X, Z]
     *  coordinates in lower and upper.
     */
    template <unsigned mask>
    Region<bitcount(mask)> subspace() const
    {
        constexpr unsigned D = bitcount(mask);
        static_assert(D <= N, "Too many dimensions");

        Region<D> out;
        unsigned j = 0;
        for (unsigned i=0; i < N; ++i) {
            if (mask & (1 << i)) {
                out.lower[j] = lower[i];
                out.upper[j] = upper[i];
                j++;
            }
        }
        out.perp.array() = 0.0;
        out.level = level;

        assert(j == D);
        return out;
    }

    /*
     *  Returns a region that is shrunk on all axes to a certain percent
     *  of the original region.  For example, shrink(1) returns the same
     *  Region; shrink(0.5) returns a region that is half the size.
     */
    Region shrink(double percentage) const
    {
        const Pt size = upper - lower;
        const Pt d = (size * (1 - percentage)) / 2.0;
        return Region(lower + d, upper - d, perp, level);
    }

    /*
     *  Returns a version of this region that has the level set
     *  based on the given minimum feature.
     *
     *  This lets us do subdivision without worrying that the termination
     *  condition (of a cell side being < min_feature) is dependent on
     *  floating-point accuracy.
     */
    Region<N> withResolution(double min_feature) const {
        const auto min_dimension = (upper - lower).minCoeff();
        const auto level = ceil(fmax(0.0, log(min_dimension / min_feature)) /
                                log(2));
        return Region<N>(lower, upper, perp, level);
    }


    /*  Finds the intersection of a ray with this region, setting *found to
     *  true on success and false otherwise. */
    Eigen::Array<double, N, 2> intersection(const Pt& pos, const Pt& dir,
                                            bool* found) const
    {
        Eigen::Array<double, N, 2> out;
        *found = false;
        // Iterate over dimensions
        for (unsigned i=0; i < N; ++i) {
            for (unsigned j=0; j < 2; ++j) {
                const double d = ((*this)[j][i] - pos[i]) / dir[i];
                const Pt pt = pos + d * dir;
                if (this->contains(pt, 0)) {
                    out.col(*found) = pt;
                    *found = true;
                }
            }
        }
        return out;
    }

    /*  Lower and upper bounds for the region  */
    Pt lower, upper;

    /*  perp is the coordinates on perpendicular axes, used when converting
     *  a 2D region into 3D coordinates for Interval evaluation  */
    Perp perp;

    /*  Used when subdividing a region to decide when to terminate;
     *  must be set beforehand with setResolution */
    int32_t level;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace libfive
