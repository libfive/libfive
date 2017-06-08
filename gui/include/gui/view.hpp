#pragma once

#include <QOpenGLWidget>

#include "gui/axes.hpp"
#include "gui/background.hpp"
#include "gui/camera.hpp"

class View : public QOpenGLWidget
{
    Q_OBJECT
public:
    View(QWidget* parent=nullptr);

protected:
    void initializeGL() override;
    void paintGL() override;
    void resizeGL(int width, int height) override;

    /*  Background items to render  */
    Axes axes;
    Background background;

    void mouseMoveEvent(QMouseEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void wheelEvent(QWheelEvent *event) override;
    Camera camera;

    struct {
        enum { RELEASED, DRAG_ROT, DRAG_PAN } state = RELEASED;
        QPoint pos;
    } mouse;
};
