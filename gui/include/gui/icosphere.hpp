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

class Icosphere : public QOpenGLFunctions
{
public:
    Icosphere();

    void draw(QMatrix4x4 M, QVector3D pos, float r);
    void initializeGL(int subdiv=4);

protected:
    QOpenGLBuffer vert_vbo;
    QOpenGLBuffer tri_vbo;
    QOpenGLVertexArrayObject vao;

    unsigned tri_count=0;
};
