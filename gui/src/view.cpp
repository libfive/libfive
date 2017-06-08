#include "gui/view.hpp"

View::View(QWidget* parent)
    : QOpenGLWidget(parent)
{
    // Nothing to do here
}

void View::initializeGL()
{
    axes.initializeGL();
    background.initializeGL();
}

void View::paintGL()
{
    background.draw();

    QMatrix4x4 M;
    axes.drawSolid(M);
    axes.drawWire(M);
}
