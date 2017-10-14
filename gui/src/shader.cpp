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

QOpenGLShaderProgram* flat;
QOpenGLShaderProgram* point;
QOpenGLShaderProgram* monochrome;
QOpenGLShaderProgram* shaded;
QOpenGLShaderProgram* busy;
QOpenGLShaderProgram* bars;
QOpenGLShaderProgram* line;

void initializeGL()
{
    flat = new QOpenGLShaderProgram;
    flat->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    flat->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    flat->link();

    monochrome = new QOpenGLShaderProgram;
    monochrome->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    monochrome->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/monochrome.frag");
    monochrome->link();

    shaded = new QOpenGLShaderProgram;
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    shaded->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/shaded.frag");
    shaded->link();

    busy = new QOpenGLShaderProgram;
    busy->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    busy->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/busy.frag");
    busy->link();

    bars = new QOpenGLShaderProgram;
    bars->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    bars->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/bars.frag");
    bars->link();

    line = new QOpenGLShaderProgram;
    line->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/line.vert");
    line->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/line.frag");
    line->link();

    point = new QOpenGLShaderProgram;
    point->addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/point.vert");
    point->addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    point->link();
}

}
