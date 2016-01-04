#include "ao/gl/texture.hpp"

void toTexture(const DepthImage& img, GLuint tex)
{
    Eigen::ArrayXXf i = img.cast<float>().transpose();

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, i.rows(), i.cols(),
            0, GL_RED, GL_FLOAT, i.data());
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
