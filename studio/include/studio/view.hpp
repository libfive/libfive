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
#pragma once

#include <QOpenGLWidget>
#include <QOpenGLFramebufferObject>

#include "studio/arrow.hpp"
#include "studio/axes.hpp"
#include "studio/background.hpp"
#include "studio/bbox.hpp"
#include "studio/bars.hpp"
#include "studio/busy.hpp"
#include "studio/camera.hpp"
#include "studio/icosphere.hpp"
#include "studio/shape.hpp"
#include "studio/settings.hpp"

#include "libfive/eval/eval_jacobian.hpp"

class View : public QOpenGLWidget, QOpenGLFunctions
{
    Q_OBJECT
public:
    View(QWidget* parent=nullptr);

    /*
     *  Requests that every active Shape cancels its render operation
     *  This is used when quitting to clean up threads quickly
     */
    void cancelShapes();

    /*
     *  In the destructor, free the OpenGL data associated with
     *  all children Shapes (because there could be shapes that
     *  aren't explicitly stored in the object, which will be freed
     *  by the QObject destructor, which runs after the OpenGL context
     *  is gone).
     */
    ~View();

public slots:
    void setShapes(QList<Shape*> shapes);
    void openSettings();
    void showAxes(bool a);
    void showBBox(bool b);

    void toOrthographic(bool=false) { camera.toOrthographic();  }
    void toPerspective(bool=false)  { camera.toPerspective();   }
    void zoomTo(bool=false) { camera.zoomTo(settings.min, settings.max); }

    /*
     *  Emits shapesReady if all the shapes being drawn
     *  are at their final (highest) resolution
     */
    void checkMeshes() const;

    /*
     *  Called when the script changes settings
     *  If first is true, then the camera zooms to the new bounds.
     */
    void onSettingsFromScript(Settings s, bool first);

    /*
     *  Called when the settings pane is edited
     */
    void onSettingsFromPane(Settings s);

    /*
     *  Enable and disable settings pane
     *  (used when exporting)
     */
    void disableSettings();
    void enableSettings();

signals:
    /*
     *  Called to kick off a render and start the busy spinner running
     */
    void startRender(Settings s);

    /*
     *  Emitted whenever settings are changed in the pane
     */
    void settingsChanged(Settings s);

    /*
     *  Emitted when all shapes are done rendering at their highest resolution
     */
    void meshesReady(QList<const Kernel::Mesh*> shapes) const;

    /*
     *  Indicates when a drag operation begins and ends
     */
    void dragStart();
    void dragEnd();

    /*
     *  Emitted when a drag operation has changed variables
     */
    void varsDragged(QMap<Kernel::Tree::Id, float> vs);

protected slots:
    void update() { QOpenGLWidget::update(); }
    void redrawPicker();

protected:
    void initializeGL() override;
    void paintGL() override;
    void resizeGL(int width, int height) override;

    /*
     *  Converts from mouse event coordinates to model coordinates
     *  using the z depth from the pick buffer
     */
    QVector3D toModelPos(QPoint pt) const;

    /*
     *  Converts from mouse event coordinates to model coordinates
     *  using a user-provided z depth
     */
    QVector3D toModelPos(QPoint pt, float z) const;

    /*  Background items to render  */
    Arrow arrow;
    Axes axes;
    Background background;
    BBox bbox;
    Busy busy;
    Bars bars;
    Icosphere ico;

    void mouseMoveEvent(QMouseEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void wheelEvent(QWheelEvent *event) override;
    void leaveEvent(QEvent* event) override;

    /*
     *  Updates hover_target based on mouse cursor position
     */
    void checkHoverTarget(QPoint pos);

    /*
     *  Ensures that the pick buffer is synced, forcing a pick render
     *  if the timer is currently running.
     */
    void syncPicker();

    Camera camera;

    struct {
        enum { RELEASED, DRAG_ROT, DRAG_PAN, DRAG_EVAL } state = RELEASED;
        QPoint pos;
    } mouse;

    QList<Shape*> shapes;
    QPointer<SettingsPane> pane;
    bool settings_enabled=true;
    Settings settings;
    bool show_axes=true;
    bool show_bbox=false;

    /*  Framebuffer to render pick data  */
    QScopedPointer<QOpenGLFramebufferObject> pick_fbo;
    QTimer pick_timer;
    QImage pick_img;
    QVector<float> pick_depth;

    /*  Data to handle direct modification of shapes */
    QVector3D drag_start;
    QVector3D drag_dir;
    QScopedPointer<Kernel::JacobianEvaluator> drag_eval;
    Shape* drag_target=nullptr;
    bool drag_valid=false;
    Shape* hover_target=nullptr;

    QVector3D cursor_pos;
    bool cursor_pos_valid=false;
};
