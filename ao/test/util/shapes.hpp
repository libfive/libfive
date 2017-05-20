#pragma once

#include <glm/mat4x4.hpp>
#include "kernel/tree/tree.hpp"

Kernel::Tree rectangle(float xmin, float xmax, float ymin, float ymax,
                       glm::mat4 M=glm::mat4());
Kernel::Tree menger(int i);
Kernel::Tree circle(float r);
Kernel::Tree sphere(float r);
