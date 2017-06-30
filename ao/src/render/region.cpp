#include <algorithm>

#include "ao/render/region.hpp"

namespace Kernel {

Region::Region(Eigen::Vector3f lower, Eigen::Vector3f upper, float res)
    : Region(lower, upper, {res, res, res})
{
    // Nothing to do here
}

Region::Region(Eigen::Vector3f _lower, Eigen::Vector3f _upper,
               Eigen::Vector3f res)
{
    size = (res.array() * (_upper - _lower).array()).ceil().max(1).cast<int>();

    auto extra = size.array().cast<float>() / res.array() -
                 (_upper - _lower).array();
    lower = _lower.array() - extra/2;
    upper = _upper.array() - extra/2;

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

Region::View Region::view() const
{
    return View(*this);
}

Region::View::View(const Region& r)
    : lower(r.lower), upper(r.upper), size(r.size), corner({0,0,0}),
      pts(r.pts[0].data(), r.pts[1].data(), r.pts[2].data())
{
    // Nothing to do here
}

Region::View::View(Eigen::Vector3f lower, Eigen::Vector3f upper,
                   Eigen::Vector3i size, Eigen::Vector3i corner,
                   Eigen::Matrix<const float*, 3, 1> pts)
    : lower(lower), upper(upper), size(size), corner(corner),
      pts(pts)
{
  // Nothing to do here
}

size_t Region::View::voxels() const
{
    return size.x() * size.y() * size.z();
}

}   // namespace Kernel
