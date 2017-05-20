#pragma once

#include <vector>

#include "kernel/eval/interval.hpp"
#include "kernel/render/subregion.hpp"

namespace Kernel {

/*
 *  A region constains X, Y, Z bounds and voxel count / positions
 */
class Region
{
public:
    /*
     *  Constructs a region with the given bounds and res voxels per unit
     *
     *  If the bounds and resolution imply fractional voxels, the bounds are
     *  expanded about their centers to include a unit number of voxels at the
     *  specified resolution.
     */
    Region(Interval x, Interval y, Interval z, float res);

    /*
     *  Constructs a region with the given bounds and per-axis resolution
     *
     *  If the bounds and resolution imply fractional voxels, the bounds are
     *  expanded about their centers to include a unit number of voxels at the
     *  specified resolution.
     */
    Region(Interval x, Interval y, Interval z,
           float rx, float ry, float rz);

    /*
     *  Returns a region with power-of-two voxel count
     *
     *  dims is the number of dimensions we care about, and must be 2 or 3
     *      If 3, a cubical region is returned
     *      If 2, a square region is returned
     */
    Region powerOfTwo(int dims) const;

    /*
     *  Return a subregion watching the full region
     */
    Subregion view() const;

    /*
     *  Helper struct that contains bounds and values for each axis
     */
    struct Axis
    {
        /* If i and res imply fractional values, the interval will be
         * expanded to fit a whole number of voxels. */
        Axis(Interval i, float res);
        Axis(Interval i, size_t size);

        /*
         *  Accessor functions for the interval object
         */
        float lower() const { return bounds.lower(); }
        float upper() const { return bounds.upper(); }

        /*  Returns a pointer to the first value  */
        const float* ptr() const { return &values[0]; }

        /*  Expands the input interval if it implies a fractional
         *  voxel count */
        static Interval expand(Interval i, float res);

        const Interval bounds;
        std::vector<float> values;
    };

    const Axis X;
    const Axis Y;
    const Axis Z;

protected:
    /*
     *  Constructs a region with the given bounds and certain size
     */
    Region(Axis x, Axis y, Axis z);

};

}   // namespace Kernel
