/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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

static constexpr unsigned bitcount(unsigned i)
{
    return (i == 0) ? 0 : ((i & 1) + bitcount(i >> 1));
}

namespace Kernel {

/*
 *  A Simplex<N, M> is an simplex in N-dimensional space
 *  Bits set in A indicate which axes are used in the simplex.
 *
 *  For example:
 *      Simplex<3, 0b111> is a cube (in 3D)
 *      Simplex<3, 0b110> is a face of a cube in XY
 *      Simplex<3, 0b001> is a cube edge parallel to the Z axis
 *      Simplex<3, 0> is a corner
 */
template <unsigned int N, unsigned A>
class Simplex
{
public:
    typedef Eigen::Array<double, bitcount(A), 1> Pt;
    typedef Eigen::Array<double, N - bitcount(A), 1> Perp;

    /*
     *  Constructs a region with the given bounds
     */
    Simplex(Pt lower, Pt upper) : lower(lower), upper(upper),
                                  perp(Perp::Zero()) {}

    /*
     *  Construct a region with the given bounds
     *  and perpendicular coordinate(s)
     */
    Simplex(Pt lower, Pt upper, Perp p) : lower(lower), upper(upper), perp(p) {}

    /*
     *  Default constructor for an empty region
     */
    Simplex() : lower(Pt::Zero()), upper(Pt::Zero()) {}

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
    std::array<Simplex, 1 << N> subdivide() const
    {
        // Default-construct empty regions
        std::array<Simplex, 1 << N> out = {};
        Pt c = (lower + upper) / 2.0;

        for (unsigned i=0; i < (1 << N); ++i)
        {
            auto a = Eigen::Array<double, N, 1>(0);
            for (unsigned j=0; j < N; ++j)
            {
                a(j) = (i & (1 << j));
            }
            out[i] = Simplex(a.select(c, lower), a.select(upper, c), perp);
        }
        return out;
    }

    Eigen::Array<double, N, 1> corner(unsigned i) const
    {
        unsigned a = 0;
        unsigned b = 0;
        Eigen::Array<double, N, 1> out;
        for (unsigned j=0; j < N; ++j)
        {
            if (A & (1 << j))
            {
                out(j) = (i & (1 << j)) ? upper(a) : lower(a);
                a++;
            }
            else
            {
                out(j) = perp(b++);
            }
        }
        return out;
    }

    Eigen::Array<double, N, 1> lowerN() const
    {
        return corner(0);
    }

    Eigen::Array<double, N, 1> upperN() const
    {
        return corner((1 << N) - 1);
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

