#pragma once

#include <functional>
#include <utility>
#include <vector>

#include "ao/eval/interval.hpp"

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
     *  Splits the region along its largest axis
     *
     *  The returned regions become invalid if their parent is destroyed
     */
    std::pair<Region, Region> split() const;

    /*
     *  Splits a region along the larger of the X and Y axes
     *
     *  The returned regions become in valid if their parent is destroyed
     */
    std::pair<Region, Region> splitXY() const;

    /*
     *  Returns true if the region can be split
     */
    bool canSplit() const;
    bool canSplitXY() const;

    /*
     *  Returns the number of voxels in this region
     */
    size_t voxels() const;

    class DiscreteRange
    {
    public:
        DiscreteRange(Interval i, float res);
        DiscreteRange(const DiscreteRange& other);
        ~DiscreteRange();

        /*
         *  Splits the region along a voxel boundary
         */
        std::pair<DiscreteRange, DiscreteRange> split() const;

        /*
         *  Accessor functions for the interval object
         */
        float lower() const { return interval.lower(); }
        float upper() const { return interval.upper(); }

        /*
         *  Returns the value at a given index.
         *
         *  pos(0)    = interval.lower()
         *  pos(size) = interval.upper()
         */
        float pos(size_t i) const { return ptr[i]; }

        const Interval interval;
        const size_t min;
        const size_t size;

        /*  This is a pointer into an array of voxel positions  */
        float* const ptr;

        /*  Root is set as true if we allocated ptr in this instance,   *
         *  false otherwise (controls whether the destructor frees it)  */
        const bool root;

    protected:
        /*
         *  Protected constructor used when splitting
         */
        DiscreteRange(Interval i, size_t min, size_t size, float* ptr);
    };

    const DiscreteRange X, Y, Z;

protected:
    /*
     *  Internal constructor used in split
     */
    Region(DiscreteRange x, DiscreteRange y, DiscreteRange z);
};

// Helper macro to iterate over a region in a deterministic order
#define REGION_ITERATE_XYZ(r) \
for (unsigned i=0; i < r.X.size; ++i)           \
    for (unsigned j=0; j < r.Y.size; ++j)       \
        for (unsigned k=0; k < r.Z.size; ++k)   \
