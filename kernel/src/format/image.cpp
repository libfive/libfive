#include <vector>

#include <png.h>

#include "ao/kernel/format/image.hpp"

namespace Image
{

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

////////////////////////////////////////////////////////////////////////////////

bool SavePng(std::string filename, const DepthImage& img)
{
    // Open up a file for writing
    FILE* output = fopen(filename.c_str(), "wb");
    if (output == NULL)
    {
        printf("Failed to open PNG file for writing (errno = %i)\n", errno);
        return false;
    }

    // Create a png pointer with the callbacks above
    png_structp png_ptr = png_create_write_struct(
        PNG_LIBPNG_VER_STRING, NULL, on_png_error, on_png_warn);
    if (png_ptr == NULL)
    {
        fprintf(stderr, "Failed to allocate png write_struct\n");
        return false;
    }

    // Create an info pointer
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL)
    {
        fprintf(stderr, "Failed to create png info_struct");
        return false;
    }

    // Set physical vars
    png_set_IHDR(png_ptr, info_ptr, img.cols(), img.rows(), 16,
                 PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_NONE,
                 PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    png_init_io(png_ptr, output);

    const float zmax = img.maxCoeff();
    const float zmin = (img == -std::numeric_limits<float>::infinity())
            .select(DepthImage::Constant(img.rows(), img.cols(), zmax),
                    img)
            .minCoeff();

    auto scaled = (zmax == zmin)
        ? DepthImage((img - zmin) + 65535)
        : DepthImage((img - zmin) * 65534 / (zmax - zmin) + 1);
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

} // namespace Image
