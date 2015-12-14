#include <limits>

#include "heightmap.hpp"
#include "tree.hpp"

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
static void recurse(Tree* t, const Region& r, Eigen::ArrayXXd& img)
{
    // Extract the block of the image that's being inspected
    auto block = img.block(r.Y.min, r.X.min, r.Y.size, r.X.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.upper()).all())
    {
        return;
    }

    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= ATOM_DOUBLE_COUNT)
    {
        pixels(t, r, img);
        return;
    }

    // Do the interval evaluation
    Interval out = t->eval(r.X.interval, r.Y.interval, r.Z.interval);

    // If strictly negative, fill up the block and return
    if (out.upper() < 0)
    {
        block = block.max(r.Z.upper());
    }
    // Otherwise, recurse if the output interval is ambiguous
    else if (out.lower() <= 0)
    {
        // Prune the tree

        // Subdivide and recurse
        assert(r.canSplit());

        auto rs = r.split();

        // Since the higher Z region is in the second item of the
        // split, evaluate rs.second then rs.first
        recurse(t, rs.second, img);
        recurse(t, rs.first, img);

        // Unprune the tree
    }
}

Eigen::ArrayXXd Render(Tree* t, Region r)
{
    auto img = Eigen::ArrayXXd(r.Y.size, r.X.size);
    img.fill(-std::numeric_limits<double>::infinity());

    recurse(t, r, img);
    return img;
}

} // namespace Heightmap
