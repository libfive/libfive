#include "ao/ui/gl/core.hpp"
#include "ao/kernel/render/heightmap.hpp"

/*
 *  Stores a DepthImage as a 1-channel depth texture in tex
 *
 *  The image should already be scaled so that -1 is closest to the front
 *  of the screen and pixels to be discarded have a depth value of 1
 */
void toDepthTexture(const DepthImage& img, GLuint tex);

/*
 *  Stores a NormalImage as a 4-channel 8-bit texture in tex
 */
void toNormalTexture(const NormalImage& img, GLuint tex);
