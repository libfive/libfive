#pragma once

#include <Eigen/Dense>

#include "ao/core/region.hpp"

class Tree;

typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> Image;

namespace Heightmap
{
/*
 *  Render a height-map image into an array of doubles (representing depth)
 */
Eigen::ArrayXXd Render(Tree* t, Region r);

/*
 *  Return a shaded image with R, G, B, A packed into int32_t pixels
 *
 *  If clip is true, voxels at the max Z height are given the normal (0, 0, 1)
 */
Image Shade(Tree* t, Region r, const Eigen::ArrayXXd& depth, bool clip=true);
}
