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
#pragma once

#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>
#include <QOpenGLFunctions>
#include <QTimer>

class Bars : public QOpenGLFunctions
{
public:
    Bars();

    void draw(const QSize& size);
    void initializeGL();

    /*
     *  Sets whether we're hovering (and thus should highlight)
     *  Returns true if changed
     */
    bool hover(bool over);

    /*  Drawn size (used in mouse-over calculations) */
    const static int side=30;

protected:
    bool hovered=false;

    QOpenGLVertexArrayObject vao;
    QOpenGLBuffer vbo;
};

