#include <QMouseEvent>

#include "gui/view.hpp"
#include "gui/shader.hpp"

View::View(QWidget* parent)
    : QOpenGLWidget(parent), camera(size()),
      settings({-10, -10, -10}, {10, 10, 10}, 10)
{
    setMouseTracking(true);
}

void View::setShape(Shape* s)
{
    shape.reset(s);
    connect(s, &Shape::gotMesh, this, [=](){ this->update(); });
    shape->startRender(settings);
}

void View::openSettings()
{
    if (pane.isNull())
    {
        pane = new SettingsPane(settings);
        connect(pane, &SettingsPane::changed,
                this, &View::onSettingsChanged);
        pane->show();
    }
}

void View::onSettingsChanged(Settings s)
{
    qDebug() << s.min << s.max << s.res;
    settings = s;
}

void View::initializeGL()
{
    Shader::initializeGL();
    axes.initializeGL();
    background.initializeGL();
}

void View::paintGL()
{
    background.draw();

    auto m = camera.M();
    if (shape)
    {
        shape->draw(m);
    }

    axes.drawSolid(m);
    axes.drawWire(m);
}

void View::resizeGL(int width, int height)
{
    camera.resize({width, height});
}

void View::mouseMoveEvent(QMouseEvent* event)
{
    QOpenGLWidget::mouseMoveEvent(event);

    if (mouse.state == mouse.DRAG_ROT)
    {
        camera.rotateIncremental(event->pos() - mouse.pos);
        update();
    }
    else if (mouse.state == mouse.DRAG_PAN)
    {
        camera.panIncremental(event->pos() - mouse.pos);
        update();
    }
    mouse.pos = event->pos();
}

void View::mousePressEvent(QMouseEvent* event)
{
    QOpenGLWidget::mousePressEvent(event);

    if (mouse.state == mouse.RELEASED)
    {
        if (event->button() == Qt::LeftButton)
        {
            mouse.state = mouse.DRAG_ROT;
        }
        else if (event->button() == Qt::RightButton)
        {
            mouse.state = mouse.DRAG_PAN;
        }
    }
}

void View::mouseReleaseEvent(QMouseEvent* event)
{
    QOpenGLWidget::mouseReleaseEvent(event);
    mouse.state = mouse.RELEASED;
}

void View::wheelEvent(QWheelEvent *event)
{
    QOpenGLWidget::wheelEvent(event);
    camera.zoomIncremental(event->angleDelta().y(), mouse.pos);
    update();
}
