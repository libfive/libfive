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

#include "libfive/render/brep/per_thread_brep.hpp"

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

    void collect(const std::vector<PerThreadBRep<N>>& children)
    {
        assert(verts.size() == 1);

        uint32_t index = 1;

        std::vector<uint32_t> pos;
        pos.resize(children.size(), 0);

        // Walk up the global vertex index, saving the appropriate vertex
        // in the array as we go.
        while (true) {
            bool found = false;
            for (unsigned i=0; i < children.size(); ++i)
            {
                if (pos[i] != children[i].indices.size() &&
                    children[i].indices.at(pos[i]) == index)
                {
                    verts.push_back(children[i].verts.at(pos[i]));
                    found = true;
                    pos[i]++;
                    index++;
                }
            }
            if (!found) {
                break;
            }
        }

        // Copy over all of the branes
        for (unsigned i=0; i < children.size(); ++i)
        {
            assert(pos[i] == children[i].indices.size());
            branes.insert(branes.end(), children[i].branes.begin(),
                                        children[i].branes.end());
        }
    }
};

}   // namespace Kernel
