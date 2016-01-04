#pragma once

#include <atomic>

#include <Eigen/Dense>

#include "ao/render/region.hpp"

class Evaluator;

typedef Eigen::Array<double, Eigen::Dynamic, Eigen::Dynamic> DepthImage;
typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> NormalImage;

namespace Heightmap
{
/*
 *  Render a height-map image into an array of doubles (representing depth)
 *  and the height-map's normals into a shaded image with R, G, B, A packed
 *  into int32_t pixels.
 */
std::pair<DepthImage, NormalImage> Render(
        Evaluator* e, Region r, const std::atomic<bool>& abort);
}
