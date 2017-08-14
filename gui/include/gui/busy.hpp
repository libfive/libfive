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
