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
#pragma once

#include <QOpenGLBuffer>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLFunctions>

class Axes : public QOpenGLFunctions
{
public:
    Axes();
    void drawSolid(QMatrix4x4 M);
    void drawWire(QMatrix4x4 M);
    void initializeGL();

protected:
    QOpenGLBuffer solid_vbo;
    QOpenGLVertexArrayObject solid_vao;

    QOpenGLBuffer wire_vbo;
    QOpenGLVertexArrayObject wire_vao;
};
