#include "ao/ui/gl/texture.hpp"

void toDepthTexture(const DepthImage& img, GLuint tex, Interval zbounds)
{
    // Map the depth buffer into the 0 - 1 range, with -inf = 1
    Eigen::ArrayXXf i = (img == -std::numeric_limits<float>::infinity())
        .select(1, (zbounds.upper() - img.cast<float>()) /
                   (zbounds.upper() - zbounds.lower())).transpose();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, i.rows(), i.cols(),
            0, GL_DEPTH_COMPONENT, GL_FLOAT, i.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

void toNormalTexture(const NormalImage& img, GLuint tex)
{
    NormalImage i = img.transpose();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Int32s are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, i.rows(), i.cols(),
            0, GL_RGBA, GL_UNSIGNED_BYTE, i.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}
