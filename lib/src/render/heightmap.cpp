#include <limits>
#include <set>

#include "ao/render/heightmap.hpp"
#include "ao/eval/result.hpp"
#include "ao/eval/evaluator.hpp"

namespace Heightmap
{

////////////////////////////////////////////////////////////////////////////////

/*
 *  Helper class that stores a queue of points to get normals for
 */
struct NormalRenderer
{
    NormalRenderer(Evaluator* e, const Region& r, NormalImage& norm)
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
        const Gradient* gs = e->evalCore<Gradient>(count);
        for (size_t i=0; i < count; ++i)
        {
            // Find the normal's length (to normalize it)
            double len = sqrt(pow(gs[i].dx, 2) +
                              pow(gs[i].dy, 2) +
                              pow(gs[i].dz, 2));

            // Pack each normal into the 0-255 range
            uint32_t dx = 255 * (gs[i].dx / (2 * len) + 0.5);
            uint32_t dy = 255 * (gs[i].dy / (2 * len) + 0.5);
            uint32_t dz = 255 * (gs[i].dz / (2 * len) + 0.5);

            // Pack the normals and a dummy alpha byte into the image
            norm(ys[i], xs[i]) = (0xff << 24) | (dz << 16) | (dy << 8) | dx;
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

    void push(size_t i, size_t j, double z)
    {
        xs[count] = r.X.min + i;
        ys[count] = r.Y.min + j;
        e->setPoint<Gradient>(Gradient(r.X.pos(i), 1, 0, 0),
                              Gradient(r.Y.pos(j), 0, 1, 0),
                              Gradient(z, 0, 0, 1),
                              count++);

        // If the gradient array is completely full, execute a
        // calculation that finds normals and blits them to the image
        if (count == NUM_POINTS)
        {
            run();
        }
    }

    Evaluator* e;
    const Region& r;
    NormalImage& norm;

    // Store the x, y coordinates of rendered points for normal calculations
    static constexpr size_t NUM_POINTS = Result::count<Gradient>();
    size_t xs[NUM_POINTS];
    size_t ys[NUM_POINTS];
    size_t count = 0;
};

////////////////////////////////////////////////////////////////////////////////

/*
*  Helper functions that evaluates a region of pixels
*/
static void pixels(Evaluator* e, const Region& r,
                   DepthImage& depth, NormalImage& norm)
{
    const double* out = e->eval(r);

    int index = 0;

    // Helper struct to render normals
    NormalRenderer nr(e, r, norm);

    // Unflatten results into the image, breaking out of loops early when a pixel
    // is written (because all subsequent pixels will be below it).  This
    // loop's behavior is dependent on how Tree::eval(Region) is structured.
    REGION_ITERATE_XYZ(r)
    {
        // If this voxel is filled (because the f-rep is less than zero)
        if (out[index++] < 0)
        {
            // Check to see whether the voxel is in front of the image's depth
            const double z = r.Z.pos(r.Z.size - k - 1);
            if (depth(r.Y.min + j, r.X.min + i) < z)
            {
                depth(r.Y.min + j, r.X.min + i) = z;

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
static void fill(Evaluator* e, const Region& r, DepthImage& depth,
                 NormalImage& norm)
{
    // Store the maximum z position (which is what we're flooding into
    // the depth image)
    const double z = r.Z.pos(r.Z.size - 1);

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
*/
static void recurse(Evaluator* e, const Region& r, DepthImage& depth,
                NormalImage& norm, const std::atomic<bool>& abort)
{
    // Stop rendering if the abort flag is set
    if (abort.load())
    {
        return;
    }

    // Extract the block of the image that's being inspected
    auto block = depth.block(r.Y.min, r.X.min, r.Y.size, r.X.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.pos(r.Z.size - 1)).all())
    {
        return;
    }

    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= Result::count<double>())
    {
        pixels(e, r, depth, norm);
        return;
    }

    // Do the interval evaluation
    Interval out = e->eval(r.X.interval, r.Y.interval, r.Z.interval);

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
        recurse(e, rs.second, depth, norm, abort);
        recurse(e, rs.first, depth, norm, abort);

        // Re-enable disabled nodes from the tree
        e->pop();
    }
}

std::pair<DepthImage, NormalImage> Render(
    Evaluator* e, Region r, const std::atomic<bool>& abort)
{
    auto depth = DepthImage(r.Y.size, r.X.size);
    auto norm = NormalImage(r.Y.size, r.X.size);

    depth.fill(-std::numeric_limits<double>::infinity());
    norm.fill(0);

    recurse(e, r, depth, norm, abort);

    // If a voxel is touching the top Z boundary, set the normal to be
    // pointing in the Z direction.
    norm = (depth == r.Z.pos(r.Z.size - 1)).select(0xffff7f7f, norm);

    return std::make_pair(depth, norm);
}

} // namespace Heightmap
