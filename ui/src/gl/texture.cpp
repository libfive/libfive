/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "ao/ui/gl/texture.hpp"

void toDepthTexture(const DepthImage& img, GLuint tex)
{
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, img.rows(), img.cols(),
            0, GL_DEPTH_COMPONENT, GL_FLOAT, img.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

void toNormalTexture(const NormalImage& img, GLuint tex)
{
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Int32s are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, img.rows(), img.cols(),
            0, GL_RGBA, GL_UNSIGNED_BYTE, img.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}
