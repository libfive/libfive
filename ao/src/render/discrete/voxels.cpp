/*
Ao: a CAD kernel for modeling with implicit functions
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
#include <algorithm>

#include "ao/render/discrete/voxels.hpp"

namespace Kernel {

Voxels::Voxels(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper,
               float res)
    : Voxels(lower, upper, {res, res, res})
{
    // Nothing to do here
}

Voxels::Voxels(const Eigen::Vector3f& _lower, const Eigen::Vector3f& _upper,
               const Eigen::Vector3f& res)
{
    Eigen::Array3i size = (res.array() * (_upper - _lower).array()).ceil().max(1).cast<int>();

    // Figure out much should be added to each axis, masking out
    // the case where resolution is zero
    auto extra = (res.array() > 0)
        .select(size.array().cast<float>() / res.array() -
                     (_upper - _lower).array(),
                Eigen::Array3i::Zero());
    lower = _lower.array() - extra/2;
    upper = _upper.array() + extra/2;

    for (int i=0; i < 3; ++i)
    {
        for (int index=0; index < size(i); ++index)
        {
            const float frac = (index + 0.5) / size(i);
            pts[i].push_back(lower(i) * (1 - frac) + upper(i) * frac);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

Voxels::View Voxels::view() const
{
    return View(*this);
}

Voxels::View::View(const Voxels& r)
    : lower(r.lower), upper(r.upper),
      size(r.pts[0].size(), r.pts[1].size(), r.pts[2].size()),
      corner({0,0,0}), pts(r.pts[0].data(), r.pts[1].data(), r.pts[2].data())
{
    // Nothing to do here
}

Voxels::View::View(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper,
                   const Eigen::Vector3i& size, const Eigen::Vector3i& corner,
                   const Eigen::Matrix<const float*, 3, 1>& pts)
    : lower(lower), upper(upper), size(size), corner(corner),
      pts(pts)
{
  // Nothing to do here
}

Voxels::View::View()
{
    // Nothing to do here
    // (invalid initialization)
}

size_t Voxels::View::voxels() const
{
    return size.x() * size.y() * size.z();
}

}   // namespace Kernel
