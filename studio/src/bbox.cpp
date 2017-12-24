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
#include <cmath>
#include <boost/math/constants/constants.hpp>

#include "studio/bbox.hpp"
#include "studio/camera.hpp"
#include "studio/shader.hpp"

void BBox::initializeGL()
{
    initializeOpenGLFunctions();

    QVector<GLfloat> data;
    auto push = [&](GLfloat x, GLfloat y){
        data.push_back(x);
        data.push_back(y);
    };

    // Build a capsule shape out of triangles
    const int res = 32;
    const float pi = boost::math::constants::pi<float>();
    for (unsigned i=0; i < 32; ++i)
    {
        auto x0 = cos(pi * i / float(res));
        auto x1 = cos(pi * (i + 1) / float(res));
        auto y0 = sin(pi * i / float(res));
        auto y1 = sin(pi * (i + 1) / float(res));

        push(x0, 1 + y0);
        push(x1, 1 + y1);
        push(0, 1);

        push(x0, -y0);
        push(x1, -y1);
        push(0, 0);
    }

    push(1, 0);
    push(1, 1);
    push(-1, 0);

    push(-1, 0);
    push(1, 1);
    push(-1, 1);

    vbo.create();
    vbo.bind();
    vbo.allocate(data.data(), data.size() * sizeof(GLfloat));

    vao.create();
    vao.bind();

    // Data stored in VAO
    glVertexAttribPointer(
            0, 2, GL_FLOAT, GL_FALSE,
            2 * sizeof(GLfloat), (GLvoid*)0);
    glEnableVertexAttribArray(0);

    vbo.release();
    vao.release();
}

void BBox::draw(const QVector3D& min, const QVector3D& max,
                const Camera& camera)
{
    auto M = camera.M();

    Shader::line->bind();
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_STENCIL_TEST);
    glStencilFuncSeparate(GL_FRONT, GL_GREATER, 1, 0xFF);
    glStencilFuncSeparate(GL_BACK, GL_NEVER, 0, 0xFF);
    glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_INCR, GL_INCR);
    glStencilOpSeparate(GL_BACK, GL_KEEP, GL_INCR, GL_INCR);

    glUniform1f(Shader::line->uniformLocation("thickness"), 0.005);
    glUniform1f(Shader::line->uniformLocation("aspect"), camera.getAspect());
    glUniform4f(Shader::line->uniformLocation("frag_color"),
                1.0f, 1.0f, 1.0f, 0.5f);

    vao.bind();

    QList<QPair<QVector3D, QVector3D>> edges = {
        {min, {min.x(), min.y(), max.z()}},
        {min, {min.x(), max.y(), min.z()}},
        {min, {max.x(), min.y(), min.z()}},

        {max, {max.x(), max.y(), min.z()}},
        {max, {max.x(), min.y(), max.z()}},
        {max, {min.x(), max.y(), max.z()}},

        {{min.x(), min.y(), max.z()}, {min.x(), max.y(), max.z()}},
        {{min.x(), min.y(), max.z()}, {max.x(), min.y(), max.z()}},

        {{max.x(), max.y(), min.z()}, {min.x(), max.y(), min.z()}},
        {{max.x(), max.y(), min.z()}, {max.x(), min.y(), min.z()}},

        {{max.x(), min.y(), min.z()}, {max.x(), min.y(), max.z()}},
        {{min.x(), max.y(), min.z()}, {min.x(), max.y(), max.z()}},
    };

    auto a_loc = Shader::line->uniformLocation("a");
    auto b_loc = Shader::line->uniformLocation("b");
    for (auto& e : edges)
    {
        auto a = (M * QVector4D(e.first, 1));
        auto b = (M * QVector4D(e.second, 1));

        a.setW(fmax(0, a.w()));
        b.setW(fmax(0, b.w()));
        glUniform3f(a_loc, a.x() / a.w(), a.y() / a.w(), a.z() / a.w());
        glUniform3f(b_loc, b.x() / b.w(), b.y() / b.w(), b.z() / b.w());

        glDrawArrays(GL_TRIANGLES, 0, 198);
    }
    vao.release();

    Shader::line->release();
    glDisable(GL_BLEND);
    glDisable(GL_STENCIL_TEST);
}

