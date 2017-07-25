#pragma once

#include <vector>
#include <Eigen/Eigen>

namespace Kernel {

template <unsigned N>
class BRep
{
public:
    BRep() { verts.push_back(Eigen::Matrix<float, N, 1>::Zero()); }

    /*  Flat array of point positions
     *  The 0th position is reserved as a marker */
    std::vector<Eigen::Matrix<float, N, 1>> verts;

    /*  [N-1]-dimensional objects (line segments, triangles) */
    std::vector<Eigen::Matrix<uint32_t, N, 1>> branes;
};

}   // namespace Kernel
