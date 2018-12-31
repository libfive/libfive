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

Kernel::Tree rectangle(double xmin, double xmax, double ymin, double ymax,
                       Eigen::Matrix4d M=Eigen::Matrix4d::Identity());
Kernel::Tree menger(int i);
Kernel::Tree circle(double r, Eigen::Vector2d pos=Eigen::Vector2d::Zero());
Kernel::Tree sphere(double r, Eigen::Vector3d center=Eigen::Vector3d::Zero());
Kernel::Tree box(const Eigen::Vector3d& lower, const Eigen::Vector3d& upper);
Kernel::Tree rotate2d(Kernel::Tree t, double angle);
Kernel::Tree move(Kernel::Tree t, Eigen::Vector3d d);
Kernel::Tree shell(Kernel::Tree t, double offset);
Kernel::Tree blend(Kernel::Tree a, Kernel::Tree b, double r);
Kernel::Tree cylinder(double r, double h, Eigen::Vector3d base=
                                        Eigen::Vector3d::Zero());
Kernel::Tree extrude(Kernel::Tree t, double lower, double upper);
Kernel::Tree sphereGyroid();
