#pragma once

#include <atomic>

#include "ao/render/region.hpp"
#include "ao/format/image.hpp"
#include "ao/tree/tree.hpp"

#include "ao/eval/evaluator.hpp"

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
        Eigen::Matrix4f M=Eigen::Matrix4f::Identity(), size_t threads=8);

std::pair<DepthImage, NormalImage> render(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort,
        Eigen::Matrix4f M=Eigen::Matrix4f::Identity());

/*
 *  Render an image using pre-allocated evaluators, returning images
 *  on the heap (for easier passing around)
 */
std::pair<DepthImage*, NormalImage*> render_(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort,
        Eigen::Matrix4f M=Eigen::Matrix4f::Identity());

}   // namespace Heightmap
}   // namespace Kernel
