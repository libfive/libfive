#pragma once

#include <atomic>

#include <Eigen/Dense>
#include <glm/mat4x4.hpp>

#include "ao/render/region.hpp"
#include "ao/gl/core.hpp"

class Tree;

typedef Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> DepthImage;
typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> NormalImage;

namespace Heightmap
{

/*
 *  Render a height-map image into an array of floats (representing depth)
 *  and the height-map's normals into a shaded image with R, G, B, A packed
 *  into int32_t pixels.
 */
std::pair<DepthImage, NormalImage> Render(
        Tree* t, Region r, const std::atomic_bool& abort,
        glm::mat4 m=glm::mat4(), size_t threads=8);
}
