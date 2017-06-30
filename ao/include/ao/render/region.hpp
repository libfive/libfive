#pragma once

#include <Eigen/Eigen>
#include <vector>
#include <array>

#include "ao/eval/interval.hpp"
#include "ao/render/axes.hpp"

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
    Region(Eigen::Vector3f lower, Eigen::Vector3f upper, float res);

    /*
     *  Constructs a region with the given bounds and per-axis resolution
     *
     *  If the bounds and resolution imply fractional voxels, the bounds are
     *  expanded about their centers to include a unit number of voxels at the
     *  specified resolution.
     */
    Region(Eigen::Vector3f lower, Eigen::Vector3f upper, Eigen::Vector3f res);

    /*  Bounding box of the region  */
    Eigen::Vector3f lower, upper;

    /*  Size in voxels  */
    Eigen::Vector3i size;

    /*  Voxel sample positions  */
    std::array<std::vector<float>, 3> pts;

    /*
     *  A Region::View is used when recursively rendering to quickly pass around
     *  position values without copying a lot of memory around.
     */
    class View
    {
    public:
        /*
         *  A subregion is constructed from a parent Region.
         *
         *  When that parent region is destroyed, all child subregions are
         *  invalidated (because their pointers point into the parent region)
         */
        View(const Region& r);

        /*
         *  Splits the region along its largest axis in A
         *  A is a bitfield of AXIS_{X,Y,Z}
         *
         *  If no available axis can be split, the second field has size = 0.
         */
        template <unsigned int A=7>
        std::pair<View, View> split() const
        {
            // Select the largest axis
            Eigen::Matrix3i::Index axis;
            (Eigen::Array3i(A & AXIS_X, A & AXIS_Y, A & AXIS_Z) != 0)
                .select(size, Eigen::Matrix3i::Zero())
                .maxCoeff(&axis);

            // Figure out the offsets, rounding down
            Eigen::Array3i size_upper = Eigen::Array3i::Zero();
            size_upper(axis) = size(axis) / 2;
            Eigen::Array3i size_lower = size.array() - size_upper;

            auto frac = size_lower.array().cast<float>() /
                        size.cast<float>().array();
            auto middle = (upper.array().cast<float>() * frac) +
                          (lower.array().cast<float>() * (1 - frac));

            auto _pts = pts;
            for (unsigned i=0; i < 3; ++i)
            {
                _pts[i] += size_lower[i];
            }
            return {View(lower, middle, size_lower, corner, pts),
                    View(middle, upper, size_upper,
                         corner.array() + size_lower, _pts)};
        }

        /*
         *  Splits this subregion into (1 << dims) other subregions
         *  For axes that cannot be split, returns a View with size = 0.
         */
        template <unsigned int N>
        std::array<View, (1<<N)> subdivide() const
        {
            // Figure out the offsets, rounding down
            Eigen::Array3i size_upper = size.array() / 2;
            Eigen::Array3i size_lower = size.array() - size_upper;

            auto frac = size_lower.array().cast<float>() /
                        size.cast<float>().array();
            auto middle = (upper.array() * frac) +
                          (lower.array() * (1 - frac));

            std::array<View, (1 << N)> out;
            for (int i=0; i < (1 << N); ++i)
            {
                auto a = Eigen::Array3i(i & AXIS_X, i & AXIS_Y, i & AXIS_Z) > 0;
                auto _lower = a.select(middle, lower);
                auto _upper = a.select(upper, middle);
                auto _size = a.select(size_upper, size_lower);
                auto _corner = a.select(size_lower, Eigen::Array3i::Zero());

                auto _pts = pts;
                for (unsigned i=0; i < 3; ++i)
                {
                    _pts[i] += size_lower[i];
                }
                out[i] = View(_lower, _upper, _size, _corner, _pts);
            }
            return out;
        }

        /*
         *  Returns the number of voxels in this region
         */
        size_t voxels() const;

        /*
         *  Checks to see whether this view is empty
         *  (e.g. any of the axes are of size 0)
         */
        bool empty() const { return size.minCoeff() == 0; }

        /*
         *  Checks to see if this view is of a unit voxel
         */
        bool unit() const { return size == Eigen::Vector3i(1, 1, 1); }

        /*  Region bounds  */
        const Eigen::Vector3f lower, upper;

        /*  Size of this subregion (in voxels)  */
        const Eigen::Vector3i size;

        /*  Lower corner in global voxel space  */
        const Eigen::Vector3i corner;

        /*  Data pointers  */
        Eigen::Array<const float*, 3, 1> pts;

    protected:
        /*
         *  Private constructor, used when subdividing
         */
        View(Eigen::Vector3f lower, Eigen::Vector3f upper,
             Eigen::Vector3i size, Eigen::Vector3i corner,
             Eigen::Matrix<const float*, 3, 1> pts);

    };

    /*
     *  Return a subregion watching the full region
     */
    View view() const;
};

}   // namespace Kernel
