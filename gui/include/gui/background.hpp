#pragma once

#include <QOpenGLBuffer>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLFunctions>

class Background : public QOpenGLFunctions
{
public:
    Background();
    void draw();
    void initializeGL();

protected:
    QOpenGLBuffer vbo;
    QOpenGLVertexArrayObject vao;
};
