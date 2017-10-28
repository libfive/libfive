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
#pragma once

#include <vector>
#include <Eigen/Eigen>
#include <Eigen/StdVector>

namespace Kernel {

template <unsigned N>
class BRep
{
public:
    BRep() {
        verts.push_back(Eigen::Matrix<float, N, 1>::Zero());
        norms.push_back(Eigen::Matrix<float, N, 1>::Zero());
        ranks.push_back(0);
    }

    /*  Flat array of point positions
     *  The 0th position is reserved as a marker */
    std::vector<Eigen::Matrix<float, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> verts;

    /*  Rank of a particular vertex
     *  The 0th position is reserved as a marker  */
    std::vector<uint8_t> ranks;

    /*  Flat array of point normals
     *  The 0th position is reserved as a marker
     *  Points with > 1 rank are assigned normals of 0, and should be
     *  drawn with flat shading in the fragment shader.
     */
    std::vector<Eigen::Matrix<float, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> norms;

    /*  [N-1]-dimensional objects (line segments, triangles) */
    std::vector<Eigen::Matrix<uint32_t, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<uint32_t, N, 1>>> branes;
};

}   // namespace Kernel
