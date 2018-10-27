/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
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
    BRep() { verts.push_back(Eigen::Matrix<float, N, 1>::Zero()); }

    /*  Flat array of point positions
     *  The 0th position is reserved as a marker */
    std::vector<Eigen::Matrix<float, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> verts;

    /*  [N-1]-dimensional objects (line segments, triangles) */
    std::vector<Eigen::Matrix<uint32_t, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<uint32_t, N, 1>>> branes;

    uint32_t pushVertex(const Eigen::Matrix<float, N, 1>& v) {
        uint32_t out = verts.size();
        verts.push_back(v);
        return out;
    }
};

}   // namespace Kernel
