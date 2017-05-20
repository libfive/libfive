#pragma once

#include <atomic>

#include <glm/mat4x4.hpp>

#include "kernel/render/region.hpp"
#include "kernel/format/image.hpp"
#include "kernel/tree/tree.hpp"

#include "kernel/eval/evaluator.hpp"

namespace Kernel {

namespace Heightmap
{

/*
 *  Render a height-map image into an array of floats (representing depth)
 *  and the height-map's normals into a shaded image with R, G, B, A packed
 *  into int32_t pixels.
 */
std::pair<DepthImage, NormalImage> render(
        const Tree t, Region r, const std::atomic_bool& abort,
        glm::mat4 m=glm::mat4(), size_t threads=8);

std::pair<DepthImage, NormalImage> render(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort,
        glm::mat4 m=glm::mat4());

/*
 *  Render an image using pre-allocated evaluators, returning images
 *  on the heap (for easier passing around)
 */
std::pair<DepthImage*, NormalImage*> render_(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort,
        glm::mat4 m=glm::mat4());

}   // namespace Heightmap
}   // namespace Kernel
