#pragma once

#include <utility>

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

    class DiscreteRange : public Interval
    {
    public:
        DiscreteRange(Interval i, double res);
        DiscreteRange(Interval i, size_t min, size_t size);

        std::pair<DiscreteRange, DiscreteRange> split() const;

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
