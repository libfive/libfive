/*
Studio: a simple GUI for the Ao CAD kernel
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
#include "gui/shader.hpp"

namespace Shader
{

QOpenGLShaderProgram* arrow;
QOpenGLShaderProgram* flat;
QOpenGLShaderProgram* point;
QOpenGLShaderProgram* monochrome;
QOpenGLShaderProgram* shaded;
QOpenGLShaderProgram* busy;
QOpenGLShaderProgram* bars;
QOpenGLShaderProgram* line;

void initializeGL()
{
    auto build = [](QString vert, QString frag) {
        auto s = new QOpenGLShaderProgram;
        s->addShaderFromSourceFile(
                QOpenGLShader::Vertex, ":/gl/" + vert + ".vert");
        s->addShaderFromSourceFile(
                QOpenGLShader::Fragment, ":/gl/" + frag + ".frag");
        s->link();
        return s;
    };

    flat = build("basic", "flat");
    monochrome = build("basic", "monochrome");
    shaded = build("basic", "shaded");
    busy = build("basic", "busy");
    bars = build("basic", "bars");
    line = build("line", "monochrome");
    point = build("point", "monochrome");
    arrow = build("arrow", "arrow");
}

}
