/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <atomic>

#include "libfive/eval/eval_heightmap.hpp"
#include "libfive/render/discrete/voxels.hpp"
#include "libfive/tree/tree.hpp"

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
            const std::vector<HeightmapEvaluator*>& es, Voxels r,
            const std::atomic_bool& abort);

    /*
     *  Saves the depth component as a 16-bit single-channel PNG
     */
    bool savePNG(std::string filename);

    typedef Eigen::Array<double, Eigen::Dynamic, Eigen::Dynamic> Depth;
    typedef Eigen::Array<uint32_t, Eigen::Dynamic, Eigen::Dynamic> Normal;

    Depth depth;
    Normal norm;

protected:
    /*
     *  Recurses down into a rendering operation
     *  Returns true if aborted, false otherwise
     */
    bool recurse(HeightmapEvaluator* e, std::shared_ptr<Tape> tape,
                 const Voxels::View& r, const std::atomic_bool& abort);

    /*
     *  Evaluates a set of voxels on a pixel-by-pixel basis
     */
    void pixels(HeightmapEvaluator* e, std::shared_ptr<Tape> tape,
                const Voxels::View& v);

    /*
     *  Fills a region of voxels, marking them as at the top of the view
     */
    void fill(HeightmapEvaluator* e, std::shared_ptr<Tape> tape,
              const Voxels::View& v);

};
}   // namespace Kernel
