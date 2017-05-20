#include <future>
#include <list>
#include <limits>
#include <set>

#include <glm/glm.hpp>

#include "kernel/render/heightmap.hpp"
#include "kernel/eval/result.hpp"
#include "kernel/eval/evaluator.hpp"

namespace Kernel {

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

        for (size_t i=0; i < count; ++i)
        {
            // Find the vector's length
            float length = sqrt(pow(ds.dx[i], 2) +
                                pow(ds.dy[i], 2) +
                                pow(ds.dz[i], 2));

            // Scale each normal into the 0-255 range
            uint32_t ix = 255 * (ds.dx[i] / (2 * length) + 0.5);
            uint32_t iy = 255 * (ds.dy[i] / (2 * length) + 0.5);
            uint32_t iz = 255 * (ds.dz[i] / (2 * length) + 0.5);

            // Pack the normals and a dummy alpha byte into the image
            norm(ys[i], xs[i]) = (0xff << 24) | (iz << 16) | (iy << 8) | ix;
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
        if (depth(r.Y.min + j, r.X.min + i) < r.Z.pos(r.Z.size - 1)) \
        for (unsigned k=0; k < r.Z.size; ++k)

/*
 *  Helper functions that evaluates a region of pixels
 */
static void pixels(Evaluator* e, const Subregion& r,
                   DepthImage& depth, NormalImage& norm)
{
    size_t index = 0;

    // Flatten the region in a particular order
    // (which needs to be obeyed by anything unflattening results)
    SUBREGION_ITERATE_XYZ(r)
    {
        e->setRaw(r.X.pos(i), r.Y.pos(j), r.Z.pos(r.Z.size - k - 1), index++);
    }
    e->applyTransform(index);

    const float* out = e->values(index);

    index = 0;

    // Helper struct to render normals
    NormalRenderer nr(e, r, norm);

    // Unflatten results into the image, breaking out of loops early when a pixel
    // is written (because all subsequent pixels will be below it).
    SUBREGION_ITERATE_XYZ(r)
    {
        // If this voxel is filled (because the f-rep is less than zero)
        if (out[index++] < 0)
        {
            // Check to see whether the voxel is in front of the image's depth
            const float z = r.Z.pos(r.Z.size - k - 1);
            if (depth(r.Y.min + j, r.X.min + i) < z)
            {
                depth(r.Y.min + j, r.X.min + i) = z;

                // Store normals to render in a bulk pass
                nr.push(i, j, z);
            }
            // Adjust the index pointer, since we can skip the rest of
            // this z-column (since future voxels are behind this one)
            index += r.Z.size - k - 1;

            break;
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
            if (depth(r.Y.min + j, r.X.min + i) < z)
            {
                depth(r.Y.min + j, r.X.min + i) = z;
                nr.push(i, j, z);
            }
        }
    }

    // Render the last of the normal calculations
    nr.flush();
}

/*
* Helper function that reduces a particular matrix block
* Returns true if finished, false if aborted
*/
static bool recurse(Evaluator* e, const Subregion& r, DepthImage& depth,
                NormalImage& norm, const std::atomic_bool& abort)
{
    // Stop rendering if the abort flag is set
    if (abort.load())
    {
        return false;
    }

    // Extract the block of the image that's being inspected
    auto block = depth.block(r.Y.min, r.X.min, r.Y.size, r.X.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.pos(r.Z.size - 1)).all())
    {
        return true;
    }

    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= Result::N)
    {
        pixels(e, r, depth, norm);
        return true;
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
        if (!recurse(e, rs.second, depth, norm, abort))
        {
            e->pop();
            return false;
        }
        if (!recurse(e, rs.first, depth, norm, abort))
        {
            e->pop();
            return false;
        }

        // Re-enable disabled nodes from the tree
        e->pop();
    }
    return true;
}

////////////////////////////////////////////////////////////////////////////////


void render(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort, glm::mat4 m,
        DepthImage& depth, NormalImage& norm)
{
    depth.fill(-std::numeric_limits<float>::infinity());
    norm.fill(0);

    // Build a list of regions by splitting on the XY axes
    std::list<Subregion> rs = {r.view()};
    while (rs.size() < es.size() && rs.front().canSplitXY())
    {
        auto f = rs.front();
        rs.pop_front();
        auto p = f.splitXY();
        rs.push_back(p.first);
        rs.push_back(p.second);
    }

    // Start a set of async tasks to render subregions in separate threads
    std::list<std::future<void>> futures;
    auto itr = es.begin();
    for (auto region : rs)
    {
        (*itr)->setMatrix(m);

        futures.push_back(std::async(std::launch::async,
            [itr, region, &depth, &norm, &abort](){
                recurse(*itr, region, depth, norm, abort);
            }));
        ++itr;
    }

    // Wait for all of the tasks to finish running in the background
    for (auto& f : futures)
    {
        f.wait();
    }

    // If a voxel is touching the top Z boundary, set the normal to be
    // pointing in the Z direction.
    norm = (depth == r.Z.values.back()).select(0xffff7f7f, norm);
}

std::pair<DepthImage, NormalImage> render(
    const Tree t, Region r, const std::atomic_bool& abort,
    glm::mat4 m, size_t workers)
{
    std::vector<Evaluator*> es;
    for (size_t i=0; i < workers; ++i)
    {
        es.push_back(new Evaluator(t));
    }

    auto out = render(es, r, abort, m);

    for (auto e : es)
    {
        delete e;
    }
    return out;
}

std::pair<DepthImage, NormalImage> render(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort, glm::mat4 m)
{
    auto depth = DepthImage(r.Y.values.size(), r.X.values.size());
    auto norm = NormalImage(r.Y.values.size(), r.X.values.size());

    render(es, r, abort, m, depth, norm);

    return std::make_pair(depth, norm);

}

std::pair<DepthImage*, NormalImage*> render_(
        const std::vector<Evaluator*>& es, Region r,
        const std::atomic_bool& abort, glm::mat4 m)
{
    auto depth = new DepthImage(r.Y.values.size(), r.X.values.size());
    auto norm = new NormalImage(r.Y.values.size(), r.X.values.size());

    render(es, r, abort, m, *depth, *norm);

    return std::make_pair(depth, norm);

}

} // namespace Heightmap

}   // namespace Kernel
