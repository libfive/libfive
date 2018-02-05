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
#include "studio/axes.hpp"
#include "studio/color.hpp"
#include "studio/shader.hpp"

Axes::Axes()
{
    // Nothing to do here
}

void Axes::initializeGL()
{
    initializeOpenGLFunctions();

    {   // In-line data for axes model
        const float o = 0.05;

        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   /********************/
                             /*    X axis        */
                             o,  o,  o,  COLOR(red),
                             o, -o,  o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o,  o, -o,  COLOR(red),
                             o, -o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o,  o,  o,  COLOR(red),
                             o,  o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o, -o,  o,  COLOR(red),
                             o, -o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),
                             /********************/
                             /*    Y axis        */
                             o,  o,  o,  COLOR(green),
                            -o,  o,  o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                             o,  o, -o,  COLOR(green),
                            -o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                             o,  o,  o,  COLOR(green),
                             o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                            -o,  o,  o,  COLOR(green),
                            -o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),
                             /********************/
                             /*    Z axis        */
                             o,  o,  o,  COLOR(blue),
                            -o,  o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                             o, -o,  o,  COLOR(blue),
                            -o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                             o,  o,  o,  COLOR(blue),
                             o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                            -o,  o,  o,  COLOR(blue),
                            -o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),
                             /********************/
                             /*    Base cube     */
                             o,  o, -o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                            -o,  o,  o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                             o, -o,  o,  1,  1,  1,

                            -o, -o, -o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                            -o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o, -o, -o,  1,  1,  1,
        };
        solid_vbo.create();
        solid_vbo.bind();
        solid_vbo.allocate(data, sizeof(data));

        solid_vao.create();
        solid_vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        solid_vbo.release();
        solid_vao.release();
    }

    {   // In-line data for wireframe model
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   0,  0,  0,  COLOR(red),
                             1,  0,  0,  COLOR(red),
                             0,  0,  0,  COLOR(green),
                             0,  1,  0,  COLOR(green),
                             0,  0,  0,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),
        };
        wire_vbo.create();
        wire_vbo.bind();
        wire_vbo.allocate(data, sizeof(data));

        wire_vao.create();
        wire_vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        wire_vbo.release();
        wire_vao.release();
    }
}

void Axes::bindShader(QMatrix4x4 M)
{
    Shader::basic->bind();
    glUniform1i(Shader::basic->uniformLocation("shading"), 0); // flat shading
    glUniform4f(Shader::basic->uniformLocation("color_mul"), 1, 1, 1, 1);
    glUniform4f(Shader::basic->uniformLocation("color_add"), 0, 0, 0, 0);
    glUniformMatrix4fv(Shader::basic->uniformLocation("M"),
                       1, GL_FALSE, M.data());
}

void Axes::drawSolid(QMatrix4x4 M)
{
    bindShader(M);
    solid_vao.bind();
    glDrawArrays(GL_TRIANGLES, 0, 54);
    solid_vao.release();

    Shader::basic->release();
}

void Axes::drawWire(QMatrix4x4 M)
{
    bindShader(M);
    glDisable(GL_DEPTH_TEST);
    wire_vao.bind();
    glDrawArrays(GL_LINES, 0, 6);
    wire_vao.release();
    glEnable(GL_DEPTH_TEST);

    Shader::basic->release();
}
