/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include "studio/background.hpp"
#include "studio/color.hpp"
#include "studio/shader.hpp"

namespace Studio {

void Background::initializeGL()
{
    initializeOpenGLFunctions();

    {
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   /********************/
                            -1, -1,  0.9,  COLOR(base03),
                            -1,  1,  0.9,  COLOR(base02),
                             1, -1,  0.9,  COLOR(base02),
                             1,  1,  0.9,  COLOR(base02),
        };
        vbo.create();
        vbo.bind();
        vbo.allocate(data, sizeof(data));

        vao.create();
        vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        vbo.release();
        vao.release();
    }
}

void Background::draw()
{
    Shader::basic->bind();
    QMatrix4x4 M;
    glUniform1i(Shader::basic->uniformLocation("shading"), 0); // flat shading
    glUniform4f(Shader::basic->uniformLocation("color_mul"), 1, 1, 1, 1);
    glUniform4f(Shader::basic->uniformLocation("color_add"), 0, 0, 0, 0);
    glUniformMatrix4fv(Shader::basic->uniformLocation("M"),
                       1, GL_FALSE, M.data());

    vao.bind();
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    vao.release();

    Shader::basic->release();
}

}   // namespace Studio
