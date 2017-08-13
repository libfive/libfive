#pragma once

#include <atomic>

#include "ao/render/discrete/voxels.hpp"
#include "ao/tree/tree.hpp"

#include "ao/eval/evaluator.hpp"

namespace Kernel {

class Heightmap
{
public:
    Heightmap(unsigned rows, unsigned cols);

    /*
     *  Render a height-map image into an array of floats (representing depth)
     *  and the height-map's normals into a shaded image with R, G, B, A packed
     *  into int32_t pixels.
     */
    static std::unique_ptr<Heightmap> render(
            const Tree t, Voxels r,
            const std::atomic_bool& abort, size_t threads=8);

    /*
     *  Render an image using pre-allocated evaluators
     */
    static std::unique_ptr<Heightmap> render(
            const std::vector<Evaluator*>& es, Voxels r,
            const std::atomic_bool& abort);

    /*
     *  Saves the depth component as a 16-bit single-channel PNG
     */
    bool savePNG(std::string filename);

    typedef Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic> Depth;
    typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> Normal;

    Depth depth;
    Normal norm;

protected:
    /*
     *  Recurses down into a rendering operation
     *  Returns true if aborted, false otherwise
     */
    bool recurse(Evaluator* e, const Voxels::View& r,
                 const std::atomic_bool& abort);

    /*
     *  Evaluates a set of voxels on a pixel-by-pixel basis
     */
    void pixels(Evaluator* e, const Voxels::View& v);

    /*
     *  Fills a region of voxels, marking them as at the top of the view
     */
    void fill(Evaluator* e, const Voxels::View& v);

};
}   // namespace Kernel
