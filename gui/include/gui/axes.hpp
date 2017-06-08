#pragma once

#include <QOpenGLBuffer>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLShaderProgram>
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

    QOpenGLShaderProgram shader;
};
