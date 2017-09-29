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
     *  Check if the given point is in the region (inclusive)
     */
    bool contains(Pt p) const
    { return (p >= (lower - 1e-8)).all() && (p <= (upper + 1e-8)).all(); }

    /*
     *  Helper function to get center of region
     */
    Pt center() const { return (lower + upper) / 2; }

    /*
     *  Constructs a region with the given bounds
     */
    Region(Pt lower, Pt upper) : lower(lower), upper(upper), perp(0) {}

    /*
     *  Construct a region with the given bounds
     *  and perpendicular coordinate(s)
     */
    Region(Pt lower, Pt upper, Perp p) : lower(lower), upper(upper), perp(p) {}

    /*
     *  Default constructor for an empty region
     */
    Region() : lower(0), upper(0) {}

    Pt& operator[](std::size_t idx)
    {
        switch(idx)
        {
            case 0: return lower;
            case 1: return upper;
            default: assert(false);
        }
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

    /*  Lower and upper bounds for the region  */
    Pt lower, upper;

    /*  perp is the coordinates on perpendicular axes, used when converting
     *  a 2D region into 3D coordinates for Interval evaluation  */
    Perp perp;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel
