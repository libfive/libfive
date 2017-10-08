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

#include <Eigen/Eigen>

#include "ao/tree/tree.hpp"

Kernel::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       Eigen::Matrix4f M=Eigen::Matrix4f::Identity());
Kernel::Tree menger(int i);
Kernel::Tree circle(float r);
Kernel::Tree sphere(float r, Eigen::Vector3f center=Eigen::Vector3f::Zero());
Kernel::Tree box(Eigen::Vector3f lower, Eigen::Vector3f upper);
Kernel::Tree rotate2d(Kernel::Tree t, float angle);
