/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/tree/tree.hpp"

Kernel::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       Eigen::Matrix4f M=Eigen::Matrix4f::Identity());
Kernel::Tree menger(int i);
Kernel::Tree circle(float r);
Kernel::Tree sphere(float r, Eigen::Vector3f center=Eigen::Vector3f::Zero());
Kernel::Tree box(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper);
Kernel::Tree rotate2d(Kernel::Tree t, float angle);
Kernel::Tree move(Kernel::Tree t, Eigen::Vector3f d);
Kernel::Tree shell(Kernel::Tree t, float offset);
Kernel::Tree blend(Kernel::Tree a, Kernel::Tree b, float r);
Kernel::Tree cylinder(float r, float h, Eigen::Vector3f base=
                                        Eigen::Vector3f::Zero());
Kernel::Tree extrude(Kernel::Tree t, float lower, float upper);
