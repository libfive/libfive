#include "ao/gl/core.hpp"
#include "ao/render/heightmap.hpp"

/*
 *  Stores a DepthImage as a 1-channel floating-point texture in tex
 */
void toTexture(const DepthImage& img, GLuint tex);

/*
 *  Stores a NormalImage as a 4-channel 8-bit texture in tex
 */
void toTexture(const NormalImage& img, GLuint tex);
