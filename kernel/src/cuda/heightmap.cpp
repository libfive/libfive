/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "ao/kernel/cuda/heightmap.hpp"
#include "ao/kernel/cuda/tape.hpp"

#include "ao/kernel/eval/evaluator.hpp"

namespace Heightmap
{

static void recurse(Evaluator* e, TapeAccelerator* a, const Subregion& r)
{
    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= a->N)
    {
        a->render(r);
        return;
    }

    // Do the interval evaluation
    Interval out = e->eval(r.X.bounds, r.Y.bounds, r.Z.bounds);

    // If strictly negative, fill up the block and return
    if (out.upper() < 0)
    {
        a->fill(r);
    }
    // Otherwise, recurse if the output interval is ambiguous
    else if (out.lower() <= 0)
    {
        // Disable inactive nodes in the tree
        e->push();

        // Subdivide and recurse
        assert(r.canSplit());

        auto rs = r.split();

        // Since the higher Z region is in the second item of the
        // split, evaluate rs.second then rs.first
        recurse(e, a, rs.second);
        recurse(e, a, rs.first);

        // Re-enable disabled nodes from the tree
        e->pop();
    }
}

DepthImage RenderCUDA(Tree* t, Region r, glm::mat4 m)
{
    auto e = Evaluator(t, m);
    auto a = TapeAccelerator(&e);

    // Allocate memory on the GPU for the image
    a.allocateImage(r);

    // Do the actual rendering
    recurse(&e, &a, r.view());

    // Read from the accelerator into a file
    std::vector<float> img = a.getImage();

    Eigen::Map<DepthImage> map(
            img.data(), r.X.values.size(), r.Y.values.size());

    return map;
}

} // namespace Heightmap
