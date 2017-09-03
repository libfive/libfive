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
    connect(&camera, &Camera::changed, this, &View::update);

    connect(&camera, &Camera::animDone, this, &View::redrawPicker);

    pick_timer.setSingleShot(true);
    pick_timer.setInterval(250);
    connect(&pick_timer, &QTimer::timeout, this, &View::redrawPicker);
}

void View::setShapes(QList<Shape*> new_shapes)
{
    // Pack tree IDs into a pair of sets for fast checking
    std::map<Kernel::Tree::Id, Shape*> new_shapes_map;
    for (auto& s : new_shapes)
    {
        new_shapes_map.insert({s->id(), s});
    }

    // Erase all existing shapes that aren't in the new_shapes list
    for (auto itr=shapes.begin(); itr != shapes.end(); /* no update */ )
    {
        auto n = new_shapes_map.find((*itr)->id());
        if (n == new_shapes_map.end())
        {
            disconnect(*itr, &Shape::gotMesh, this, &View::update);
            (*itr)->deleteLater();
            itr = shapes.erase(itr);
        }
        else
        {
            (*itr)->updateFrom(n->second);
            new_shapes_map.erase(n);
            ++itr;
        }
    }

    // Start up the busy spinner
    if (new_shapes_map.size())
    {
        busy.show();
    }

    // Connect all new shapes
    for (auto s : new_shapes)
    {
        if (new_shapes_map.find(s->id()) != new_shapes_map.end())
        {
            connect(s, &Shape::gotMesh, this, &View::update);
            connect(s, &Shape::gotMesh, this, &View::checkMeshes);
            connect(s, &Shape::gotMesh, this, &View::redrawPicker);
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

void View::openSettings()
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
    initializeOpenGLFunctions();

    Shader::initializeGL();

    axes.initializeGL();
    background.initializeGL();
    busy.initializeGL();
    bars.initializeGL();
}

void View::redrawPicker()
{
    // Rebuild buffer if it is not present or is the wrong size
    if (!pick_fbo.data() ||  pick_fbo->size() != camera.size)
    {
        bool needs_gl = (context() == QOpenGLContext::currentContext());
        if (needs_gl)
        {
            makeCurrent();
        }

        pick_fbo.reset(new QOpenGLFramebufferObject(
                    camera.size, QOpenGLFramebufferObject::Depth));

        if (needs_gl)
        {
            doneCurrent();
        }
    }

    pick_fbo->bind();

    glClearColor(0, 0, 0, 1);
    glClearDepthf(1);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    glViewport(0, 0, camera.size.width(), camera.size.height());

    auto m = camera.M();
    QRgb color = 255;
    for (auto& s : shapes)
    {
        s->drawMonochrome(m, color);
        color = color << 8;
    }

    auto i = pick_fbo->toImage();

    pick_fbo->release();
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

    // Draw hamburger menu
    bars.draw(camera.size);
}

void View::resizeGL(int width, int height)
{
    camera.size = {width, height};
    pick_timer.start();
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
    else if (bars.hover(event->pos().x() > camera.size.width() - bars.side &&
                        event->pos().y() < bars.side))
    {
        update();
    }
    mouse.pos = event->pos();
}

void View::mousePressEvent(QMouseEvent* event)
{
    QOpenGLWidget::mousePressEvent(event);

    // Force update of pick buffer if the timer is running
    // e.g. if the user resizes the window, then immediately
    // clicks, we should handle it correctly.
    if (pick_timer.isActive())
    {
        pick_timer.stop();
        redrawPicker();
    }

    if (mouse.state == mouse.RELEASED)
    {
        if (event->pos().x() > camera.size.width() - bars.side &&
            event->pos().y() < bars.side)
        {
            openSettings();
        }
        else if (event->button() == Qt::LeftButton)
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
    if (mouse.state != mouse.RELEASED)
    {
        redrawPicker();
    }
    mouse.state = mouse.RELEASED;
}

void View::wheelEvent(QWheelEvent *event)
{
    QOpenGLWidget::wheelEvent(event);
    camera.zoomIncremental(event->angleDelta().y(), mouse.pos);
    update();
    pick_timer.start();
}

void View::leaveEvent(QEvent* event)
{
    QOpenGLWidget::leaveEvent(event);
    if (bars.hover(false))
    {
        update();
    }
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
