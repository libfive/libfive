#include "ao/gl/core.hpp"
#include "ao/render/heightmap.hpp"

/*
 *  Stores a DepthImage as a 1-channel depth texture in tex
 */
void toDepthTexture(const DepthImage& img, GLuint tex,
                    Interval zbounds={-1, 1});

/*
 *  Stores a NormalImage as a 4-channel 8-bit texture in tex
 */
void toNormalTexture(const NormalImage& img, GLuint tex);
