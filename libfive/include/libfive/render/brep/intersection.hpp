/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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
