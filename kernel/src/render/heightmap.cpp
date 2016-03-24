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
#include <future>
#include <list>
#include <limits>
#include <set>

#include <glm/glm.hpp>

#include "ao/kernel/render/heightmap.hpp"
#include "ao/kernel/eval/result.hpp"
#include "ao/kernel/eval/evaluator.hpp"

#ifdef USE_CUDA
#include "ao/kernel/eval/accelerator.hpp"
#endif

namespace Heightmap
{

////////////////////////////////////////////////////////////////////////////////

/*
 *  Helper class that stores a queue of points to get normals for
 */
struct NormalRenderer
{
    NormalRenderer(Evaluator* e, const Subregion& r, NormalImage& norm)
        : e(e), r(r), norm(norm) {}

    /*
     *  Assert on destruction that the normals were flushed
     */
    ~NormalRenderer()
    {
        assert(count == 0);
    }

    void run()
    {
        // Get derivative array pointers
        auto ds = e->derivs(count);
        auto dx = std::get<1>(ds);
        auto dy = std::get<2>(ds);
        auto dz = std::get<3>(ds);

        for (size_t i=0; i < count; ++i)
        {
            // Find the vector's length
            float length = sqrt(pow(dx[i], 2) + pow(dy[i], 2) + pow(dz[i], 2));

            // Scale each normal into the 0-255 range
            uint32_t ix = 255 * (dx[i] / (2 * length) + 0.5);
            uint32_t iy = 255 * (dy[i] / (2 * length) + 0.5);
            uint32_t iz = 255 * (dz[i] / (2 * length) + 0.5);

            // Pack the normals and a dummy alpha byte into the image
            norm(xs[i], ys[i]) = (0xff << 24) | (iz << 16) | (iy << 8) | ix;
        }
        count = 0;
    }

    void flush()
    {
        if (count > 0)
        {
            run();
        }
    }

    void push(size_t i, size_t j, float z)
    {
        xs[count] = r.X.min + i;
        ys[count] = r.Y.min + j;
        e->set(r.X.pos(i), r.Y.pos(j), z, count++);

        // If the gradient array is completely full, execute a
        // calculation that finds normals and blits them to the image
        if (count == NUM_POINTS)
        {
            run();
        }
    }

    Evaluator* e;
    const Subregion& r;
    NormalImage& norm;

    // Store the x, y coordinates of rendered points for normal calculations
    static constexpr size_t NUM_POINTS = Result::N;
    size_t xs[NUM_POINTS];
    size_t ys[NUM_POINTS];
    size_t count = 0;
};

////////////////////////////////////////////////////////////////////////////////

// Helper macro to iterate over a region in a deterministic order
#define SUBREGION_ITERATE_XYZ(r) \
for (unsigned i=0; i < r.X.size; ++i)           \
    for (unsigned j=0; j < r.Y.size; ++j)       \
        for (unsigned k=0; k < r.Z.size; ++k)

/*
 *  Helper functions that evaluates a region of pixels
 */
#ifdef USE_CUDA
static void pixels(Evaluator* e, Accelerator* a,
#else
static void pixels(Evaluator* e,
#endif
                   const Subregion& r,
                   DepthImage& depth, NormalImage& norm)
{
    size_t index = 0;

    // Flatten the region in a particular order
    // (which needs to be obeyed by anything unflattening results)
#ifdef USE_CUDA
    a->setRegion(r);
#else
    SUBREGION_ITERATE_XYZ(r)
    {
        e->set(r.X.pos(i), r.Y.pos(j), r.Z.pos(r.Z.size - k - 1), index++);
    }
#endif

#ifdef USE_CUDA
    const float* out = a->fromDevice(a->values(r.voxels()));
#else
    const float* out = e->values(r.voxels());
#endif

    index = 0;

    // Helper struct to render normals
    NormalRenderer nr(e, r, norm);

    // Unflatten results into the image, breaking out of loops early when a pixel
    // is written (because all subsequent pixels will be below it).  This
    // loop's behavior is dependent on how Tree::eval(Region) is structured.
    SUBREGION_ITERATE_XYZ(r)
    {
        // If this voxel is filled (because the f-rep is less than zero)
        if (out[index++] < 0)
        {
            // Check to see whether the voxel is in front of the image's depth
            const float z = r.Z.pos(r.Z.size - k - 1);
            if (depth(r.X.min + i, r.Y.min + j) < z)
            {
                depth(r.X.min + i, r.Y.min + j) = z;

                // Adjust the index pointer, since we can skip the rest of
                // this z-column (since future voxels are behind this one)
                index += r.Z.size - k - 1;

                // Store normals to render in a bulk pass
                nr.push(i, j, z);
                break;
            }
        }
    }

    // Render the last of the normal calculations
    nr.flush();
}

/*
 *  Fills the given region with depth = zmax,
 *  calculating normals as appropriate
 *
 *  This function is used when marking an Interval as filled
 */
static void fill(Evaluator* e, const Subregion& r, DepthImage& depth,
                 NormalImage& norm)
{
    // Store the maximum z position (which is what we're flooding into
    // the depth image)
    const float z = r.Z.pos(r.Z.size - 1);

    // Helper struct to handle normal rendering
    NormalRenderer nr(e, r, norm);

    // Iterate over every pixel in the region
    for (unsigned i=0; i < r.X.size; ++i)
    {
        for (unsigned j=0; j < r.Y.size; ++j)
        {
            // Check to see whether the voxel is in front of the image's depth
            if (depth(r.X.min + i, r.Y.min + j) < z)
            {
                depth(r.X.min + i, r.Y.min + j) = z;
                nr.push(i, j, z);
            }
        }
    }

    // Render the last of the normal calculations
    nr.flush();
}

/*
* Helper function that reduces a particular matrix block
*/
#ifdef USE_CUDA
static void recurse(Evaluator* e, Accelerator* a,
#else
static void recurse(Evaluator* e,
#endif
                    const Subregion& r, DepthImage& depth,
                    NormalImage& norm, const std::atomic_bool& abort)
{
    // Stop rendering if the abort flag is set
    if (abort.load())
    {
        return;
    }

    // Extract the block of the image that's being inspected
    auto block = depth.block(r.X.min, r.Y.min, r.X.size, r.Y.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.pos(r.Z.size - 1)).all())
    {
        return;
    }

    // If we're below a certain size, render pixel-by-pixel
#ifdef USE_CUDA
    if (r.voxels() <= Accelerator::N)
    {
        pixels(e, a, r, depth, norm);
#else
    if (r.voxels() <= Result::N)
    {
        pixels(e, r, depth, norm);
#endif
        return;
    }

    // Do the interval evaluation
    Interval out = e->eval(r.X.bounds, r.Y.bounds, r.Z.bounds);

    // If strictly negative, fill up the block and return
    if (out.upper() < 0)
    {
        fill(e, r, depth, norm);
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
#ifdef USE_CUDA
        recurse(e, a, rs.second, depth, norm, abort);
        recurse(e, a, rs.first, depth, norm, abort);
#else
        recurse(e, rs.second, depth, norm, abort);
        recurse(e, rs.first, depth, norm, abort);
#endif

        // Re-enable disabled nodes from the tree
        e->pop();
    }
}

std::pair<DepthImage, NormalImage> Render(
    Tree* t, Region r, const std::atomic_bool& abort,
    glm::mat4 m, size_t workers)
{
    auto depth = DepthImage(r.X.values.size(), r.Y.values.size());
    auto norm = NormalImage(r.X.values.size(), r.Y.values.size());

    depth.fill(-std::numeric_limits<float>::infinity());
    norm.fill(0);

#ifdef USE_CUDA
    (void)workers; /* unused */
    auto eval = Evaluator(t, m);
    auto accel = Accelerator(&eval);
    recurse(&eval, &accel, r.view(), depth, norm, abort);
#else
    // Build a list of regions by splitting on the XY axes
    std::list<Subregion> rs = {r.view()};
    while (rs.size() < workers && rs.front().canSplitXY())
    {
        auto f = rs.front();
        rs.pop_front();
        auto p = f.splitXY();
        rs.push_back(p.first);
        rs.push_back(p.second);
    }

    // Start a set of async tasks to render subregions in separate threads
    std::list<std::future<void>> futures;
    for (auto region : rs)
    {
        auto e = new Evaluator(t, m);

        futures.push_back(std::async(std::launch::async,
            [e, region, &depth, &norm, &abort](){
                recurse(e, region, depth, norm, abort);
                delete e;
            }));
    }

    // Wait for all of the tasks to finish running in the background
    for (auto& f : futures)
    {
        f.wait();
    }
#endif

    // If a voxel is touching the top Z boundary, set the normal to be
    // pointing in the Z direction.
    norm = (depth == r.Z.values.back()).select(0xffff7f7f, norm);

    return std::make_pair(depth, norm);
}

} // namespace Heightmap
