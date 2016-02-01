#pragma once

#include <atomic>

#include <glm/mat4x4.hpp>

#include "ao/kernel/render/region.hpp"
#include "ao/kernel/format/image.hpp"

class Tree;

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
