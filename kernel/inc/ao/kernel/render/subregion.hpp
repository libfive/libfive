#pragma once

#include <vector>
#include <utility>

#include "ao/eval/interval.hpp"

class Region;

/*
 *  A Subregion is a view into a Region, used when recursively rendering
 *  to quickly get at position values without copying a lot of memory around.
 */
class Subregion
{
public:
    /*
     *  A subregion is constructed from a parent Region.
     *
     *  When that parent region is destroyed, all child subregions are
     *  invalidated (because their pointers point into the parent region)
     */
    Subregion(const Region& r);

    /*
     *  Splits the region along its largest axis
     *
     *  The returned regions become invalid if their parent is destroyed
     */
    std::pair<Subregion, Subregion> split() const;

    /*
     *  Splits a region along the larger of the X and Y axes
     *
     *  The returned regions become in valid if their parent is destroyed
     */
    std::pair<Subregion, Subregion> splitXY() const;

    /*
     *  Returns true if the region can be split
     */
    bool canSplit() const;
    bool canSplitXY() const;

    /*
     *  Splits this subregion into 8 other subregions
     *
     *  This function may only be called with dimensions are all equal
     *  and all divisible by 2
     */
    std::array<Subregion, 8> octsect() const;

    /*
     *  Returns the number of voxels in this region
     */
    size_t voxels() const;

    class Axis
    {
    public:
        Axis(Interval bounds, const std::vector<float>& vs);

        /*
         *  Splits the region along a voxel boundary
         */
        std::pair<Axis, Axis> split() const;

        /*
         *  Accessor functions for the interval object
         */
        float lower() const { return bounds.lower(); }
        float upper() const { return bounds.upper(); }

        /*
         *  Returns the value at a given index.
         */
        float pos(size_t i) const { return ptr[i]; }

        /*  Bounds of this axis  */
        const Interval bounds;

        /*  This is a pointer into an array of voxel positions  */
        const float* const ptr;

        /*  min is the position within the global voxel space  */
        const size_t min;

        /*  size is the number of voxels in the space  */
        const size_t size;

    protected:
        /*
         *  Private constructor used when splitting Axis
         */
        Axis(Interval bounds, const float* values,
                      size_t size, size_t min);
    };

    const Axis X, Y, Z;

protected:
    /*
     *  Private constructor used when spliting
     */
    Subregion(const Axis& x, const Axis& y, const Axis& z);
};

// Helper macro to iterate over a region in a deterministic order
#define SUBREGION_ITERATE_XYZ(r) \
for (unsigned i=0; i < r.X.size; ++i)           \
    for (unsigned j=0; j < r.Y.size; ++j)       \
        for (unsigned k=0; k < r.Z.size; ++k)   \
