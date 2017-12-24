/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

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
#include <vector>
#include <array>

#include "libfive/eval/interval.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

/*
 *  A Voxels object constains X, Y, Z bounds and voxel count / positions
 */
class Voxels
{
public:
    /*
     *  Constructs a region with the given bounds and res voxels per unit
     *
     *  If the bounds and resolution imply fractional voxels, the bounds are
     *  expanded about their centers to include a unit number of voxels at the
     *  specified resolution.
     */
    Voxels(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper, float res);

    /*
     *  Constructs a region with the given bounds and per-axis resolution
     *
     *  If the bounds and resolution imply fractional voxels, the bounds are
     *  expanded about their centers to include a unit number of voxels at the
     *  specified resolution.
     */
    Voxels(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper,
           const Eigen::Vector3f& res);

    /*  Bounding box of the region  */
    Eigen::Vector3f lower, upper;

    /*  Voxel sample positions  */
    std::array<std::vector<float>, 3> pts;

    /*
     *  A Voxels::View is used when recursively rendering to quickly pass around
     *  position values without copying a lot of memory around.
     */
    class View
    {
    public:
        /*
         *  A subregion is constructed from a parent Voxels.
         *
         *  When that parent region is destroyed, all child subregions are
         *  invalidated (because their pointers point into the parent region)
         */
        View(const Voxels& r);

        /*
         *  Splits the region along its largest axis in A
         *  A is a bitfield of Axis::{X,Y,Z}
         *
         *  If no available axis can be split, the second field has size = 0.
         */
        template <unsigned int A=7>
        std::pair<View, View> split() const
        {
            // Select the largest axis
            Eigen::Array3i::Index axis;
            (Eigen::Array3i(A & Axis::X, A & Axis::Y, A & Axis::Z) != 0)
                .select(size, Eigen::Vector3i::Zero())
                .maxCoeff(&axis);

            // Figure out the offsets, rounding down
            Eigen::Array3i size_upper = size;
            Eigen::Array3i size_lower = size;
            size_upper(axis) = size(axis) / 2;
            size_lower(axis) -= size_upper(axis);

            auto frac = size_lower(axis) / float(size(axis));
            float middle = (upper(axis) * frac) + (lower(axis) * (1 - frac));

            auto upper_lower = lower;
            auto lower_upper = upper;

            upper_lower(axis) = middle;
            lower_upper(axis) = middle;

            auto upper_pts = pts;
            upper_pts(axis) += size_lower(axis);
            auto upper_corner = corner;
            upper_corner(axis) += size_lower(axis);

            return {View(lower, lower_upper, size_lower, corner, pts),
                    View(upper_lower, upper, size_upper,
                         upper_corner, upper_pts)};
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
        Eigen::Vector3f lower, upper;

        /*  Size of this subregion (in voxels)  */
        Eigen::Vector3i size;

        /*  Lower corner in global voxel space  */
        Eigen::Vector3i corner;

        /*  Data pointers  */
        Eigen::Array<const float*, 3, 1> pts;

    protected:
        /*
         *  Private constructor, used when subdividing
         */
        View(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper,
             const Eigen::Vector3i& size, const Eigen::Vector3i& corner,
             const Eigen::Matrix<const float*, 3, 1>& pts);

        View();
    };

    /*
     *  Return a subregion watching the full region
     */
    View view() const;
};

}   // namespace Kernel
