/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <atomic>

#include <Eigen/Eigen>
#include <Eigen/StdVector>

namespace libfive {

/*
 *  A PerThreadBRep is a thread-safe class used when to a BRep from multiple
 *  threads simultaneously.  Construct a set of PerThreadBReps (one per thread)
 *  with the same atomic counter, then collect them with BRep::collect to merge
 *  them into a single model.
 */
template <unsigned N>
class PerThreadBRep
{
public:
    PerThreadBRep(std::atomic<uint32_t>& c) : c(c)
    {
        assert(c.load() == 1);
    }

    uint32_t pushVertex(const Eigen::Matrix<float, N, 1>& v) {
        const auto out = c.fetch_add(1);
        this->verts.push_back(v);
        indices.push_back(out);
        return out;
    }
    uint32_t pushVertex(const Eigen::Matrix<double, N, 1>& v) {
        const Eigen::Matrix<float, N, 1> v_ = v.template cast<float>();
        return pushVertex(v_);
    }

    /*  Adds a debug line to the mesh, drawing it as a zero-area
     *  triangle.  This is inefficient and ignores any indexing,
     *  so should only be used for debugging purposes */
    void drawDebugLine(const Eigen::Matrix<float, N, 1>& a,
                       const Eigen::Matrix<float, N, 1>& b)
    {
        const auto a_ = pushVertex(a);
        const auto b_ = pushVertex(b);
        this->branes.push_back({a_, b_, a_});
    }

    std::vector<Eigen::Matrix<float, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<float, N, 1>>> verts;
    std::vector<Eigen::Matrix<uint32_t, N, 1>,
                Eigen::aligned_allocator<Eigen::Matrix<uint32_t, N, 1>>> branes;
    std::vector<uint32_t> indices;

protected:
    std::atomic<uint32_t>& c;
};

}   // namespace libfive
