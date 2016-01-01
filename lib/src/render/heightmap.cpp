#include <limits>

#include "ao/render/heightmap.hpp"
#include "ao/core/tree.hpp"
#include "ao/gl/accelerator.hpp"

#include <iostream>

namespace Heightmap
{

/*
 *  Helper functions that evaluates a region of pixels
 */
static void pixels(Tree* t, const Region& r, Eigen::ArrayXXd& img)
{
    const double* out = t->eval(r);

    int index = 0;

    // Unflatten results into the image, breaking out of loops early when a pixel
    // is written (because all subsequent pixels will be below it).  This
    // loop's behavior is dependent on how Tree::eval(Region) is structured.
    REGION_ITERATE_XYZ(r)
    {
        if (out[index++] < 0)
        {
            const double z = r.Z.pos(r.Z.size - k - 1);
            if (img(r.Y.min + j, r.X.min + i) < z)
            {
                img(r.Y.min + j, r.X.min + i) = z;
                index += r.Z.size - k - 1;
                break;
            }
        }
    }
}

/*
 * Helper function that reduces a particular matrix block
 */
static void recurse(Tree* t, const Region& r, Eigen::ArrayXXd& img,
                    const std::atomic<bool>& abort)
{
    // Stop rendering if the abort flag is set
    if (abort.load())
    {
        return;
    }

    // Extract the block of the image that's being inspected
    auto block = img.block(r.Y.min, r.X.min, r.Y.size, r.X.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.pos(r.Z.size - 1)).all())
    {
        return;
    }

    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= Result::count<double>())
    {
        pixels(t, r, img);
        return;
    }

    // Do the interval evaluation
    Interval out = t->eval(r.X.interval, r.Y.interval, r.Z.interval);

    // If strictly negative, fill up the block and return
    if (out.upper() < 0)
    {
        block = block.max(r.Z.pos(r.Z.size - 1));
    }
    // Otherwise, recurse if the output interval is ambiguous
    else if (out.lower() <= 0)
    {
        // Disable inactive nodes in the tree
        t->push();

        // Subdivide and recurse
        assert(r.canSplit());

        auto rs = r.split();

        // Since the higher Z region is in the second item of the
        // split, evaluate rs.second then rs.first
        recurse(t, rs.second, img, abort);
        recurse(t, rs.first, img, abort);

        // Re-enable disabled nodes from the tree
        t->pop();
    }
}

Eigen::ArrayXXd Render(Tree* t, Region r, const std::atomic<bool>& abort)
{
    auto img = Eigen::ArrayXXd(r.Y.size, r.X.size);
    img.fill(-std::numeric_limits<double>::infinity());

    if (auto accel = t->getAccelerator())
    {
        accel->makeContextCurrent();
        accel->Render(r, img);
    }
    else
    {
        recurse(t, r, img, abort);
    }

    return img;
}

Image Shade(Tree* t, Region r, const Eigen::ArrayXXd& depth,
            const std::atomic<bool>& abort, bool clip)
{
    auto img = Image(r.Y.size, r.X.size);
    img.fill(0);

    // Store the x, y coordinates of rendered points
    constexpr size_t NUM_POINTS = Result::count<Gradient>();
    size_t xs[NUM_POINTS];
    size_t ys[NUM_POINTS];
    size_t index = 0;

    auto run = [&](){
        const Gradient* gs = t->evalCore<Gradient>(index);
        for (size_t i=0; i < index; ++i)
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
            img(ys[i], xs[i]) = (0xff << 24) | (dz << 16) | (dy << 8) | dx;
        }
        index = 0;
    };

    const double zmax = r.Z.pos(r.Z.size - 1);

    for (int row=0; row < depth.rows(); ++row)
    {
        // Escape out of the loop if the abort flag is set
        if (abort.load())
        {
            break;
        }

        const double y = r.Y.pos(row);
        for (int col=0; col < depth.cols(); ++col)
        {
            const double d = depth(row, col);

            // If the pixel is touching the top Z boundary, set
            // its normal to be solely in the Z direction.
            if (clip && d == zmax)
            {
                img(row, col) = 0xffff7f7f;
            }
            else if (!isinf(d))
            {
                xs[index] = col;
                ys[index] = row;
                t->setPoint<Gradient>(Gradient(r.X.pos(col), 1, 0, 0),
                                      Gradient(y, 0, 1, 0),
                                      Gradient(d, 0, 0, 1), index++);
                if (index == NUM_POINTS)
                {
                    run();
                }
            }
        }
    }
    if (index > 0)
    {
        run();
    }
    return img;
}

} // namespace Heightmap
