#include <limits>

#include "heightmap.hpp"
#include "tree.hpp"

#include <iostream>

namespace Heightmap
{

/*
 *  Helper functions that evaluates a region of pixels
 */
static void pixels(Tree* t, Region r, Eigen::ArrayXXd& img)
{
    const double* out = t->eval(r);

    int index = 0;
    r.forEach([&](size_t i, size_t j, size_t k)
            {
                // Get this voxel's z position
                const double z = r.Z.pos(k);

                // If the voxel is filled and higher than the current pixel
                // in the heightmap, fill it in with the new z height
                if (out[index] < 0 && img(r.Y.min + j, r.X.min + i) < z)
                {
                    img(r.Y.min + j, r.X.min + i) = z;
                }
                index++;
            });
}

/*
 * Helper function that reduces a particular matrix block
 */
static void recurse(Tree* t, Region r, Eigen::ArrayXXd& img)
{
    // Extract the block of the image that's being inspected
    auto block = img.block(r.Y.min, r.X.min, r.Y.size, r.X.size);

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.Z.upper()).all())
    {
        return;
    }

    // If we're below a certain size, render pixel-by-pixel
    if ((r.X.size + 1) * (r.Z.size + 1) * (r.Z.size + 1) <= ATOM_DOUBLE_COUNT)
    {
        pixels(t, r, img);
        return;
    }

    // Do the interval evaluation
    Interval out; // = t->eval(r.X.interval, r.Y.interval, r.Z.interval);

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
    auto img = Eigen::ArrayXXd(r.Y.size + 1, r.X.size + 1);
    img.fill(-std::numeric_limits<double>::infinity());

    recurse(t, r, img);
    return img;
}

} // namespace Heightmap
