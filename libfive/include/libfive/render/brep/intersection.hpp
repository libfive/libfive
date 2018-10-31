/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>
#include <Eigen/StdVector>

#include <boost/container/small_vector.hpp>

namespace Kernel {

template <unsigned N>
struct Intersection {
    Eigen::Matrix<double, N, 1> pos;
    Eigen::Matrix<double, N, 1> deriv;
    double value;
    uint32_t index; // Unique per-vertex index when unpacking into a b-rep.
                    // Only used by the first intersection in each vec.
                    // Not checked in equality comparison.
    bool operator==(const Intersection& other) const;
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template <unsigned N>
bool inline Intersection<N>::operator==(const Intersection<N>& other) const
{
    return pos == other.pos && deriv == other.deriv && value == other.value;
}

template <size_t N>
using IntersectionVec =
        boost::container::small_vector<Intersection<N>, 4,
            Eigen::aligned_allocator<Intersection<N>>>;

}   // namespace Kernel
