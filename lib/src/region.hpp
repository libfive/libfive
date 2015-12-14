#pragma once

#include <functional>
#include <utility>
#include <vector>

#include "interval.hpp"

/*
 *  A region constains X, Y, Z bounds and voxel count / positions
 */
class Region
{
public:
    /*
     *  Constructs a region with the given bounds and res voxels per unit
     */
    Region(Interval x, Interval y, Interval z, double res);

    /*
     *  Splits the region along its largest axis
     */
    std::pair<Region, Region> split() const;

    /*
     *  Returns true if the region can be split
     */
    bool canSplit() const;

    /*
     *  Returns the number of voxels in this region
     */
    size_t voxels() const;

    /*
     *  Calls a function on each voxel in the region
     *
     *  Used to iterate over the region's voxels in deterministic order
     */
    void forEach(std::function<void(size_t, size_t, size_t)> f) const;

    class DiscreteRange
    {
    public:
        DiscreteRange(Interval i, double res);
        DiscreteRange(Interval i, size_t min, size_t size);

        /*
         *  Splits the region along a voxel boundary
         */
        std::pair<DiscreteRange, DiscreteRange> split() const;

        /*
         *  Accessor functions for the interval object
         */
        double lower() const { return interval.lower(); }
        double upper() const { return interval.upper(); }

        /*
         *  Returns the value at a given index.
         *
         *  pos(0)    = interval.lower()
         *  pos(size) = interval.upper()
         */
        double pos(size_t i) const;

        const Interval interval;
        const size_t min;
        const size_t size;
    };

    const DiscreteRange X, Y, Z;

protected:
    /*
     *  Internal constructor used in split
     */
    Region(DiscreteRange x, DiscreteRange y, DiscreteRange z);
};
