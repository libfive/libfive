#pragma once

#include <Eigen/Dense>

#include "ao/core/region.hpp"

class Tree;

namespace Heightmap
{
Eigen::ArrayXXd Render(Tree* t, Region r);
}
