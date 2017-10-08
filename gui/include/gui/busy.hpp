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

#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>
#include <QOpenGLFunctions>
#include <QTimer>

class Busy : public QObject, QOpenGLFunctions
{
    Q_OBJECT
public:
    Busy();

    void draw(const QSize& size);
    void initializeGL();
signals:
    void redraw();

public slots:
    void show();
    void hide();

protected:
    mutable QTimer timer;

    Q_PROPERTY(float fade MEMBER fade)
    float fade;

    QOpenGLVertexArrayObject vao;
    QOpenGLBuffer vbo;
};
