#include "ao/gl/texture.hpp"

void toTexture(const DepthImage& img, GLuint tex)
{
    // Scale the texture, mapping [-1,1] to [1,0] (with -inf == 1)
    auto i = img.cast<float>().transpose();
    Eigen::ArrayXXf j = (i == -std::numeric_limits<float>::infinity())
        .select(1, (1 - i) / 2);

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, j.rows(), j.cols(),
            0, GL_DEPTH_COMPONENT, GL_FLOAT, j.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}

void toTexture(const NormalImage& img, GLuint tex)
{
    NormalImage i = img.transpose();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Int32s are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, i.rows(), i.cols(),
            0, GL_RGBA, GL_UNSIGNED_BYTE, i.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}
