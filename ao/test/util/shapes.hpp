#pragma once

#include <Eigen/Eigen>

#include "ao/tree/tree.hpp"

Kernel::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       Eigen::Matrix4f M=Eigen::Matrix4f::Identity());
Kernel::Tree menger(int i);
Kernel::Tree circle(float r);
Kernel::Tree sphere(float r, Eigen::Vector3f center=Eigen::Vector3f::Zero());
Kernel::Tree box(Eigen::Vector3f lower, Eigen::Vector3f upper);
Kernel::Tree CylinderYAxis(Eigen::Vector3f start, float r);
Kernel::Tree rotate2d(Kernel::Tree t, float angle);
