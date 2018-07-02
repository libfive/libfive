/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include <QMouseEvent>
#include <QMessageBox>
#include <QPainter>

#include "studio/color.hpp"
#include "studio/view.hpp"
#include "studio/shader.hpp"

#include "libfive/solve/solver.hpp"

View::View(QWidget* parent)
    : QOpenGLWidget(parent), camera(size()),
      settings(Settings::defaultSettings())
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

View::~View()
{
    makeCurrent();
    for (auto s : findChildren<Shape*>())
    {
        s->freeGL();
    }
    doneCurrent();
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
    bool vars_changed = false;
    for (auto itr=shapes.begin(); itr != shapes.end(); /* no update */ )
    {
        auto n = new_shapes_map.find((*itr)->id());
        if (n == new_shapes_map.end())
        {
            if (*itr == drag_target)
            {
                drag_target->setGrabbed(false);
                drag_target = nullptr;
                emit(dragEnd());
                mouse.state = mouse.RELEASED;
            }
            if (*itr == hover_target)
            {
                hover_target->setHover(false);
                hover_target = nullptr;
            }
            disconnect(*itr, &Shape::redraw, this, &View::update);
            (*itr)->deleteLater();
            itr = shapes.erase(itr);
            pick_timer.start();
        }
        else
        {
            vars_changed |= (*itr)->updateFrom(n->second);
            new_shapes_map.erase(n);
            ++itr;
        }
    }

    // Start up the busy spinner
    if (new_shapes_map.size() || vars_changed)
    {
        busy.show();
    }

    // Connect all new shapes
    for (auto s : new_shapes)
    {
        if (new_shapes_map.find(s->id()) != new_shapes_map.end())
        {
            connect(s, &Shape::redraw, this, &View::update);
            connect(s, &Shape::gotMesh, this, &View::checkMeshes);
            connect(s, &Shape::gotMesh, &pick_timer,
                    static_cast<void (QTimer::*)()>(&QTimer::start));
            connect(this, &View::startRender,
                    s, static_cast<void (Shape::*)(Settings)>(&Shape::startRender));
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

void View::onSettingsFromScript(Settings s, bool first)
{
    if (settings != s)
    {
        settings = s;
        startRender(s);
    }

    if (first)
    {
        camera.zoomTo(s.min, s.max);
    }
}

void View::initializeGL()
{
    initializeOpenGLFunctions();

    Shader::initializeGL();

    arrow.initializeGL();
    axes.initializeGL();
    background.initializeGL();
    bbox.initializeGL();
    busy.initializeGL();
}

void View::redrawPicker()
{
    // Only begin redrawing the pick buffer once we've been drawn once
    // and confirmed that the OpenGL context is new enough to work.
    if (!gl_checked)
    {
        pick_timer.start();
        return;
    }

    // We may not have the OpenGL context, so we claim it here
    // (and release it at the bottom if it was claimed)
    const bool needs_gl = (context() != QOpenGLContext::currentContext());
    if (needs_gl)
    {
        makeCurrent();
    }

    // Rebuild buffer if it is not present or is the wrong size
    if (!pick_fbo.data() ||  pick_fbo->size() != camera.size)
    {
        pick_fbo.reset(new QOpenGLFramebufferObject(
                    camera.size, QOpenGLFramebufferObject::Depth));
    }

    pick_fbo->bind();

    glClearColor(0, 0, 0, 1);
    glClearDepthf(1);
    glClearStencil(0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    glViewport(0, 0, camera.size.width(), camera.size.height());

    auto m = camera.M();
    QRgb color = 1;
    for (auto& s : shapes)
    {
        s->drawMonochrome(m, color++);
    }

    pick_img = pick_fbo->toImage();

    // There's no utility function to get the depth buffer, so we manually
    // read it with glReadPixels here.
    if (pick_depth.size() != camera.size.width() * camera.size.height())
    {
        pick_depth.resize(camera.size.width() * camera.size.height());
    }
    glReadPixels(0, 0, camera.size.width(), camera.size.height(),
                 GL_DEPTH_COMPONENT, GL_FLOAT, pick_depth.data());

    glDisable(GL_DEPTH_TEST);
    pick_fbo->release();

    if (needs_gl)
    {
        doneCurrent();
    }
}

void View::paintGL()
{
    if (!gl_checked)
    {
        auto def = QSurfaceFormat::defaultFormat();
        auto fmt = context()->format();
        if (fmt.majorVersion() < def.majorVersion() ||
                (fmt.majorVersion() == def.majorVersion() &&
                 fmt.minorVersion() < def.minorVersion()))
        {
            auto err = QString(
                    "Error:<br><br>"
                    "OpenGL context is too old<br>"
                    "(got %1.%2, need %3.%4)<br><br>"
                    "The application will now exit.<br>")
                    .arg(fmt.majorVersion())
                    .arg(fmt.minorVersion())
                    .arg(def.majorVersion())
                    .arg(def.minorVersion());

            QMessageBox::critical(this, "Studio", err);
            exit(1);
        }
        gl_checked = true;
    }

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    painter.beginNativePainting();
    background.draw();

    auto m = camera.M();
    glEnable(GL_DEPTH_TEST);

    for (auto& s : shapes)
    {
        s->draw(m);
    }

    if (show_axes)
    {
        axes.drawSolid(m);
        axes.drawWire(m);
    }

    if (show_bbox)
    {
        // This is intentionally accidentally quadratic, as we won't
        // be drawing too many bounding boxes and QVector3D doesn't
        // come with operator< or qHash overloads.
        QList<QPair<QVector3D, QVector3D>> shown;
        auto draw_bbox = [&](QVector3D min, QVector3D max){
            if (!shown.contains({min, max}))
            {
                bbox.draw(min, max, camera);
                shown.push_back({min, max});
            }
        };
        for (auto& s : shapes)
        {
            auto b = s->getRenderBounds();
            draw_bbox(QVector3D(b.lower.x(), b.lower.y(), b.lower.z()),
                      QVector3D(b.upper.x(), b.upper.y(), b.upper.z()));
        }
        if (shapes.size() == 0)
        {
            draw_bbox(settings.min, settings.max);
        }
    }

    if (drag_target)
    {
        arrow.draw(m, cursor_pos, 0.1 / camera.getScale(),
                   drag_dir.normalized(),
                   drag_valid ? Color::green : Color::red);
    }

    // This is a no-op if the spinner is hidden
    busy.draw(camera.size);

    glDisable(GL_DEPTH_TEST);
    painter.endNativePainting();

    if (cursor_pos_valid)
    {
        QFont font = painter.font();
        font.setFamily("Courier");
        painter.setFont(font);

        painter.setBrush(Qt::NoBrush);
        painter.setPen(Color::base1);
        painter.drawText(QPointF(10, camera.size.height() - 40),
                         QString("X: %1%2").arg(cursor_pos.x() < 0 ? "" : " ").arg(cursor_pos.x()));
        painter.drawText(QPointF(10, camera.size.height() - 25),
                         QString("Y: %1%2").arg(cursor_pos.y() < 0 ? "" : " ").arg(cursor_pos.y()));
        painter.drawText(QPointF(10, camera.size.height() - 10),
                         QString("Z: %1%2").arg(cursor_pos.z() < 0 ? "" : " ").arg(cursor_pos.z()));
    }
}

void View::resizeGL(int width, int height)
{
    camera.size = {width, height};
    pick_timer.start();
}

void View::syncPicker()
{
    if (pick_timer.isActive())
    {
        pick_timer.stop();
        redrawPicker();
    }
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
    else if (mouse.state == mouse.DRAG_EVAL)
    {
        // Convert to 3D coordinates, then find the 3D ray that's
        // represented by the mouse position + viewing angle
        cursor_pos = toModelPos(event->pos(), 0);
        auto ray = cursor_pos - toModelPos(event->pos(), 1);

        // Slide pos down the ray to minimize distance to drag start
        cursor_pos += ray * QVector3D::dotProduct(drag_start - cursor_pos, ray);

        // Solve for the point on the normal ray that is closest to the cursor ray
        // https://en.wikipedia.org/wiki/Skew_lines#Distance_between_two_skew_lines
        const auto n = QVector3D::crossProduct(drag_dir, ray);
        const auto n2 = QVector3D::crossProduct(ray, n);
        cursor_pos = drag_start +
            drag_dir * QVector3D::dotProduct(cursor_pos - drag_start, n2) /
            QVector3D::dotProduct(drag_dir, n2);

        auto sol = Kernel::Solver::findRoot(
                *drag_eval.first, drag_eval.second,
                drag_target->getVars(),
                {cursor_pos.x(), cursor_pos.y(), cursor_pos.z()});
        emit(varsDragged(QMap<Kernel::Tree::Id, float>(sol.second)));

        drag_valid = fabs(sol.first) < 1e-6;
        bool changed = false;
        for (auto& s : shapes)
        {
            changed |= s->updateVars(sol.second);
        }
        if (changed)
        {
            busy.show();
        }
    }
    else
    {
        checkHoverTarget(event->pos());
    }
    mouse.pos = event->pos();
}

QVector3D View::toModelPos(QPoint pt) const
{
    float pick_z = 0;
    if (pick_img.valid(pt))
    {
        pick_z = 2 * pick_depth.at(
                    pt.x() + pick_img.width() *
                    (pick_img.height() - pt.y() - 1)) - 1;
    }
    return toModelPos(pt, pick_z);
}

QVector3D View::toModelPos(QPoint pt, float z) const
{
    return camera.M().inverted() * QVector3D(
            (pt.x() * 2.0) / pick_img.width() - 1,
            1 - (pt.y() * 2.0) / pick_img.height(), z);
}

void View::mousePressEvent(QMouseEvent* event)
{
    QOpenGLWidget::mousePressEvent(event);

    if (mouse.state == mouse.RELEASED)
    {
        if (event->button() == Qt::LeftButton)
        {
            syncPicker();
            auto picked = (pick_img.pixel(event->pos()) & 0xFFFFFF);
            drag_target = picked ? shapes.at(picked - 1) : nullptr;
            if (picked && drag_target->hasVars())
            {
                this->setCursor(Qt::ClosedHandCursor);
                emit(dragStart());
                drag_target->setGrabbed(true);
                drag_valid = true;

                drag_start = toModelPos(event->pos());
                auto df = drag_target->dragFrom(drag_start);
                drag_eval.first.reset(df.first);
                drag_eval.second = df.second;

                auto norm = drag_eval.first->deriv(
                        {drag_start.x(), drag_start.y(), drag_start.z()},
                        df.second);
                drag_dir = {norm.x(), norm.y(), norm.z()};

                mouse.state = mouse.DRAG_EVAL;
            }
            else
            {
                drag_target = nullptr;
                mouse.state = mouse.DRAG_ROT;
            }
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
        if (drag_target)
        {
            drag_target->setGrabbed(false);
            drag_target = nullptr;
            emit(dragEnd());
            checkHoverTarget(event->pos());
        }
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

void View::checkHoverTarget(QPoint pos)
{
    syncPicker();

    auto picked = pick_img.valid(pos) ? (pick_img.pixel(pos) & 0xFFFFFF) : 0;
    auto target = picked ? shapes.at(picked - 1) : nullptr;
    if (target && target->hasVars())
    {
        if (hover_target)
        {
            hover_target->setHover(false);
        }

        hover_target = target;
        hover_target->setHover(true);
    }
    else if (hover_target)
    {
        hover_target->setHover(false);
        hover_target = nullptr;
    }

    bool changed = (cursor_pos_valid != (bool)target);
    cursor_pos_valid = target;
    cursor_pos = toModelPos(pos);
    if (cursor_pos_valid || changed)
    {
        update();
    }

    this->setCursor(hover_target ? Qt::OpenHandCursor : Qt::ArrowCursor);
}

void View::showAxes(bool a)
{
    show_axes = a;
    update();
}

void View::showBBox(bool b)
{
    show_bbox = b;
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
