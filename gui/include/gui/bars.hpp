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

