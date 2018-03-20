/*
libfive: a CAD kernel for modeling with implicit functions
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

#include <Eigen/Eigen>

namespace Kernel {

struct Bezier
{
    Bezier(const Eigen::Vector3f& a,
           const Eigen::Vector3f& b,
           const Eigen::Vector3f& c)
        : a(a), b(b), c(c)
    {   /* Nothing to do here */ }

    template <class T>
    T at(T t, int axis) const {
       return pow(1 - t, 2) * a(axis) +
              2 * (1 - t) * t * b(axis) +
              pow(t, 2) * c(axis);
    }

    template <class T>
    Eigen::Matrix<T, 3, 1> at(T t) const {
        return {at(t, 0), at(t, 1), at(t, 2)};
    }

    const Eigen::Vector3f a;
    const Eigen::Vector3f b;
    const Eigen::Vector3f c;
};

}   // namespace Kernel
