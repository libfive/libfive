#pragma once

#include <QOpenGLBuffer>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLFunctions>

class BBox : public QOpenGLFunctions
{
public:
    void draw(const QVector3D& min, const QVector3D& max,
              const QMatrix4x4& M, float scale);
    void initializeGL();

protected:
    QOpenGLBuffer vbo;
    QOpenGLVertexArrayObject vao;
};
