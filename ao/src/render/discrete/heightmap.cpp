#include <iostream>
#include <future>
#include <list>
#include <limits>
#include <set>

#include <boost/algorithm/string/predicate.hpp>
#include <png.h>

#include "ao/render/discrete/heightmap.hpp"
#include "ao/eval/result.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////

/*
 *  Helper class that stores a queue of points to get normals for
 */
struct NormalRenderer
{
    NormalRenderer(Evaluator* e, const Voxels::View& r, Heightmap::Normal& norm)
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
        xs[count] = r.corner.x() + i;
        ys[count] = r.corner.y() + j;
        e->set({r.pts.x()[i], r.pts.y()[j], z}, count++);

        // If the gradient array is completely full, execute a
        // calculation that finds normals and blits them to the image
        if (count == NUM_POINTS)
        {
            run();
        }
    }

    Evaluator* e;
    const Voxels::View& r;
    Heightmap::Normal& norm;

    // Store the x, y coordinates of rendered points for normal calculations
    static constexpr size_t NUM_POINTS = Result::N;
    size_t xs[NUM_POINTS];
    size_t ys[NUM_POINTS];
    size_t count = 0;
};

////////////////////////////////////////////////////////////////////////////////

// Helper macro to iterate over a region in a deterministic order
#define VIEW_ITERATE_XYZ(r) \
for (int i=0; i < r.size.x(); ++i)           \
    for (int j=0; j < r.size.y(); ++j)       \
        if (depth(r.corner.y() + j, r.corner.x() + i) < r.pts.z()[r.size.z() - 1]) \
        for (int k=0; k < r.size.z(); ++k)

/*
 *  Helper functions that evaluates a region of pixels
 */
void Heightmap::pixels(Evaluator* e, const Voxels::View& r)
{
    size_t index = 0;

    // Flatten the region in a particular order
    // (which needs to be obeyed by anything unflattening results)
    VIEW_ITERATE_XYZ(r)
    {
        e->setRaw({r.pts.x()[i], r.pts.y()[j], r.pts.z()[r.size.z() - k - 1]},
                   index++);
    }
    e->applyTransform(index);

    const float* out = e->values(index);

    index = 0;

    // Helper struct to render normals
    NormalRenderer nr(e, r, norm);

    // Unflatten results into the image, breaking out of loops early when a pixel
    // is written (because all subsequent pixels will be below it).
    VIEW_ITERATE_XYZ(r)
    {
        // If this voxel is filled (because the f-rep is less than zero)
        if (out[index++] < 0)
        {
            // Check to see whether the voxel is in front of the image's depth
            const float z = r.pts.z()[r.size.z() - k - 1];
            if (depth(r.corner.y() + j, r.corner.x() + i) < z)
            {
                depth(r.corner.y() + j, r.corner.x() + i) = z;

                // Store normals to render in a bulk pass
                nr.push(i, j, z);
            }
            // Adjust the index pointer, since we can skip the rest of
            // this z-column (since future voxels are behind this one)
            index += r.size.z() - k - 1;

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
void Heightmap::fill(Evaluator* e, const Voxels::View& r)
{
    // Store the maximum z position (which is what we're flooding into
    // the depth image)
    const float z = r.pts.z()[r.size.z() - 1];

    // Helper struct to handle normal rendering
    NormalRenderer nr(e, r, norm);

    // Iterate over every pixel in the region
    for (int i=0; i < r.size.x(); ++i)
    {
        for (int j=0; j < r.size.y(); ++j)
        {
            // Check to see whether the voxel is in front of the image's depth
            if (depth(r.corner.y() + j, r.corner.x() + i) < z)
            {
                depth(r.corner.y() + j, r.corner.x() + i) = z;
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
bool Heightmap::recurse(Evaluator* e, const Voxels::View& r,
                        const std::atomic_bool& abort)
{
    // Stop rendering if the abort flag is set
    if (abort.load())
    {
        return false;
    }

    // Extract the block of the image that's being inspected
    auto block = depth.block(r.corner.y(), r.corner.x(),
                             r.size.y(), r.size.x());

    // If all points in the region are below the heightmap, skip it
    if ((block >= r.pts.z()[r.size.z() - 1]).all())
    {
        return true;
    }

    // If we're below a certain size, render pixel-by-pixel
    if (r.voxels() <= Result::N)
    {
        pixels(e, r);
        return true;
    }

    // Do the interval evaluation
    Interval::I out = e->eval(r.lower, r.upper);

    // If strictly negative, fill up the block and return
    if (Interval::isFilled(out))
    {
        fill(e, r);
    }
    // Otherwise, recurse if the output interval is ambiguous
    else if (!Interval::isEmpty(out))
    {
        // Disable inactive nodes in the tree
        e->push();

        auto rs = r.split();

        // Since the higher Z region is in the second item of the
        // split, evaluate rs.second then rs.first
        if (!recurse(e, rs.second, abort))
        {
            e->pop();
            return false;
        }
        if (!recurse(e, rs.first, abort))
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

Heightmap::Heightmap(unsigned rows, unsigned cols)
    : depth(rows, cols), norm(rows, cols)
{
    // Nothing to do here
}

std::unique_ptr<Heightmap> Heightmap::render(
    const Tree t, Voxels r, const std::atomic_bool& abort,
    Eigen::Matrix4f m, size_t workers)
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

std::unique_ptr<Heightmap> Heightmap::render(
        const std::vector<Evaluator*>& es, Voxels r,
        const std::atomic_bool& abort, Eigen::Matrix4f m)
{
    auto out = new Heightmap(r.pts[1].size(), r.pts[0].size());

    out->depth.fill(-std::numeric_limits<float>::infinity());
    out->norm.fill(0);

    // Build a list of regions by splitting on the XY axes
    std::list<Voxels::View> rs = {r.view()};
    while (rs.size() < es.size() && rs.front().size.head<2>().minCoeff() > 1)
    {
        auto f = rs.front();
        rs.pop_front();
        auto p = f.split<Axis::X | Axis::Y>();
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
            [itr, region, &out, &abort](){
                out->recurse(*itr, region, abort);
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
    out->norm = (out->depth == r.pts[2].back()).select(0xffff7f7f, out->norm);

    return std::unique_ptr<Heightmap>(out);
}

////////////////////////////////////////////////////////////////////////////////

static void on_png_error(png_structp p, png_const_charp msg)
{
    (void)p; // unused
    fprintf(stderr, "libpng error with message '%s'\n", msg);
}

static void on_png_warn(png_structp p, png_const_charp msg)
{
    (void)p; // unused
    fprintf(stderr, "libpng warning with message '%s'\n", msg);
}

bool Heightmap::savePNG(std::string filename)
{
    if (!boost::algorithm::iends_with(filename, ".png"))
    {
        std::cerr << "Heightmap::savePNG: filename \"" << filename
                  << "\" does not end in .png" << std::endl;
    }

    // Open up a file for writing
    FILE* output = fopen(filename.c_str(), "wb");
    if (output == NULL)
    {
        fprintf(stderr, "Failed to open PNG file for writing (errno = %i)\n",
                errno);
        return false;
    }

    // Create a png pointer with the callbacks above
    png_structp png_ptr = png_create_write_struct(
        PNG_LIBPNG_VER_STRING, NULL, on_png_error, on_png_warn);
    if (png_ptr == NULL)
    {
        fprintf(stderr, "Failed to allocate png write_struct\n");
        fclose(output);
        return false;
    }

    // Create an info pointer
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL)
    {
        fprintf(stderr, "Failed to create png info_struct");
        png_destroy_write_struct(&png_ptr, &info_ptr);
        fclose(output);
        return false;
    }

    // Set physical vars
    png_set_IHDR(png_ptr, info_ptr, depth.cols(), depth.rows(), 16,
                 PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                 PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    png_init_io(png_ptr, output);

    const float zmax = depth.maxCoeff();
    const float zmin = (depth == -std::numeric_limits<float>::infinity())
            .select(Depth::Constant(depth.rows(), depth.cols(), zmax),
                    depth)
            .minCoeff();

    auto scaled = (zmax == zmin)
        ? Depth((depth - zmin) + 65535)
        : Depth((depth - zmin) * 65534 / (zmax - zmin) + 1);
    Eigen::Array<uint16_t, Eigen::Dynamic, Eigen::Dynamic>
        pixels = scaled.cast<uint16_t>().transpose();

    std::vector<uint16_t*> rows;
    for (int i=pixels.cols() - 1; i >= 0; --i)
    {
        rows.push_back(pixels.data() + i * pixels.rows());
    }

    png_set_rows(png_ptr, info_ptr, reinterpret_cast<png_bytepp>(&rows[0]));
    png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_SWAP_ENDIAN, NULL);
    fclose(output);

    png_destroy_write_struct(&png_ptr, &info_ptr);
    return true;
}

}   // namespace Kernel
