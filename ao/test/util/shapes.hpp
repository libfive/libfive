#pragma once

#include <Eigen/Eigen>

#include "ao/tree/tree.hpp"

Kernel::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       Eigen::Matrix4f M=Eigen::Matrix4f::Identity());
Kernel::Tree menger(int i);
Kernel::Tree circle(float r);
Kernel::Tree sphere(float r);
Kernel::Tree box(Eigen::Vector3f lower, Eigen::Vector3f upper);
