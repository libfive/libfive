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

libfive::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       Eigen::Matrix4f M=Eigen::Matrix4f::Identity());
libfive::Tree menger(int i);
libfive::Tree circle(float r, Eigen::Vector2f pos=Eigen::Vector2f::Zero());
libfive::Tree sphere(float r, Eigen::Vector3f center=Eigen::Vector3f::Zero());
libfive::Tree box(const Eigen::Vector3f& lower, const Eigen::Vector3f& upper);
libfive::Tree rotate2d(libfive::Tree t, float angle);
libfive::Tree rotate_x(libfive::Tree t, float angle);
libfive::Tree move(libfive::Tree t, Eigen::Vector3f d);
libfive::Tree shell(libfive::Tree t, float offset);
libfive::Tree blend(libfive::Tree a, libfive::Tree b, float r);
libfive::Tree cylinder(float r, float h, Eigen::Vector3f base=
                                         Eigen::Vector3f::Zero());
libfive::Tree extrude(libfive::Tree t, float lower, float upper);
libfive::Tree sphereGyroid();
