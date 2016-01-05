#include "ao/gl/core.hpp"
#include "ao/render/heightmap.hpp"

/*
 *  Stores a DepthImage as a 1-channel depth texture in tex
 *  Assumes that the relevant range is [-1, 1]
 */
void toDepthTexture(const DepthImage& img, GLuint tex);

/*
 *  Stores a NormalImage as a 4-channel 8-bit texture in tex
 */
void toNormalTexture(const NormalImage& img, GLuint tex);

/*
 *  Converts a normal texture into an image
 */
NormalImage fromNormalTexture(GLuint tex, const Region& r);

/*
 *  Converts a depth texture into an image, mapping depth to
 *  the given region's z boundaries
 */
DepthImage fromDepthTexture(GLuint tex, const Region& r);
