#pragma once

#include <QOpenGLBuffer>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLExtraFunctions>

class Camera;

class BBox : public QOpenGLExtraFunctions
{
public:
    void draw(const QVector3D& min, const QVector3D& max,
              const Camera& camera);
    void initializeGL();

protected:
    QOpenGLBuffer vbo;
    QOpenGLVertexArrayObject vao;
};
