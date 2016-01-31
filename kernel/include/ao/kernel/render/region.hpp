#pragma once

#include <vector>

#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/render/subregion.hpp"

/*
 *  A region constains X, Y, Z bounds and voxel count / positions
 */
class Region
{
public:
    /*
     *  Constructs a region with the given bounds and res voxels per unit
     */
    Region(Interval x, Interval y, Interval z, float res);

    /*
     *  Constructs a region with the given bounds and per-axis resolution
     */
    Region(Interval x, Interval y, Interval z,
           float rx, float ry, float rz);

    /*
     *  Returns a cubical region with power-of-two voxel count
     */
    Region powerOfTwo() const;

    /*
     *  Return a subregion watching the full region
     */
    Subregion view() const;

    /*
     *  Helper struct that contains bounds and values for each axis
     */
    struct Axis
    {
        Axis(Interval i, float res);
        Axis(Interval i, size_t size);

        /*  Returns a pointer to the first value  */
        const float* ptr() const { return &values[0]; }

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
