#include <QMouseEvent>

#include "gui/view.hpp"
#include "gui/shader.hpp"

View::View(QWidget* parent)
    : QOpenGLWidget(parent), camera(size()),
      settings({-10, -10, -10}, {10, 10, 10}, 10, 8)
{
    setMouseTracking(true);

    connect(&busy, &Busy::redraw, this, &View::update);
    connect(this, &View::startRender, &busy,
            [&](Settings){ busy.show(); });
    connect(this, &View::meshesReady, &busy,
            [&](QList<const Kernel::Mesh*>){ busy.hide(); });
}

void View::setShapes(QList<Shape*> new_shapes)
{
    // Pack tree IDs into a pair of sets for fast checking
    QSet<Kernel::Tree::Id> new_shape_ids;
    for (auto& s : new_shapes)
    {
        new_shape_ids.insert(s->id());
    }

    // Erase all existing shapes that aren't in the new_shapes list
    for (auto itr=shapes.begin(); itr != shapes.end(); /* no update */ )
    {
        if (new_shape_ids.find((*itr)->id()) == new_shape_ids.end())
        {
            disconnect(*itr, &Shape::gotMesh, this, &View::update);
            (*itr)->deleteLater();
            itr = shapes.erase(itr);
        }
        else
        {
            new_shape_ids.remove((*itr)->id());
            ++itr;
        }
    }

    // Start up the busy spinner
    if (new_shape_ids.size())
    {
        busy.show();
    }

    // Connect all new shapes
    for (auto s : new_shapes)
    {
        if (new_shape_ids.find(s->id()) != new_shape_ids.end())
        {
            connect(s, &Shape::gotMesh, this, &View::update);
            connect(s, &Shape::gotMesh, this, &View::checkMeshes);
            connect(this, &View::startRender,
                    s, &Shape::startRender);
            s->startRender(settings);
            s->setParent(this);

            shapes.push_back(s);
        }
        else
        {
            s->deleteLater();
        }
    }
    update();
}

void View::cancelShapes()
{
    for (auto& s : shapes)
    {
        s->deleteLater();
    }
    shapes.clear();
}

void View::openSettings(bool)
{
    if (pane.isNull())
    {
        pane = new SettingsPane(settings);
        connect(pane, &SettingsPane::changed,
                this, &View::onSettingsFromPane);
        pane->show();
        pane->setFixedSize(pane->size());
    }
    else
    {
        pane->setFocus();
    }
}

void View::onSettingsFromPane(Settings s)
{
    settings = s;
    startRender(s);
    emit(settingsChanged(s));
}

void View::onSettingsFromScript(Settings s)
{
    if (settings != s)
    {
        if (pane.isNull())
        {
            settings = s;
            startRender(s);
        }
        else
        {
            // This ends up calling onSettingsFromPane if anything has changed
            pane->set(s);
        }
    }
}

void View::initializeGL()
{
    Shader::initializeGL();

    axes.initializeGL();
    background.initializeGL();
    busy.initializeGL();
}

void View::paintGL()
{
    background.draw();

    auto m = camera.M();
    for (auto& s : shapes)
    {
        s->draw(m);
    }

    if (show_axes)
    {
        axes.drawSolid(m);
        axes.drawWire(m);
    }

    // This is a no-op if the spinner is hidden
    busy.draw(camera.size);
}

void View::resizeGL(int width, int height)
{
    camera.size = {width, height};
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

void View::showAxes(bool a)
{
    show_axes = a;
    update();
}

void View::checkMeshes() const
{
    bool all_done = true;
    QList<const Kernel::Mesh*> meshes;
    for (auto s : shapes)
    {
        if (s->done())
        {
            meshes.push_back(s->getMesh());
        }
        else
        {
            all_done = false;
        }
    }
    if (all_done)
    {
        emit(meshesReady(meshes));
    }
}
