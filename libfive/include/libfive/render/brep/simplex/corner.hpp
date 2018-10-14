/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <cstdint>
#include <Eigen/Eigen>
#include <boost/container/small_vector.hpp>

namespace Kernel {

// Used to build simplex QEF matrices
template <unsigned N>
struct Corner
{
    float value;
    boost::container::small_vector<Eigen::Matrix<double, N, 1>, 1,
        Eigen::aligned_allocator<Eigen::Matrix<double, N, 1>>> deriv;
};

template <unsigned N>
using CornerArray = std::array<Corner<N>, 1 << N>;

}   // namespace Kernel
