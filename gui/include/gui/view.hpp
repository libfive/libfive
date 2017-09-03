#pragma once

#include <QOpenGLWidget>
#include <QOpenGLFramebufferObject>

#include "gui/axes.hpp"
#include "gui/background.hpp"
#include "gui/bars.hpp"
#include "gui/busy.hpp"
#include "gui/camera.hpp"
#include "gui/shape.hpp"
#include "gui/settings.hpp"

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

public slots:
    void setShapes(QList<Shape*> shapes);
    void openSettings();
    void showAxes(bool a);

    void toOrthographic(bool=false) { camera.toOrthographic();  }
    void toPerspective(bool=false)  { camera.toPerspective();   }

    /*
     *  Emits shapesReady if all the shapes being drawn
     *  are at their final (highest) resolution
     */
    void checkMeshes() const;

    /*
     *  Called when the script changes settings
     */
    void onSettingsFromScript(Settings s);

    /*
     *  Called when the settings pane is edited
     */
    void onSettingsFromPane(Settings s);

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
     *  Emitted when the window size changes
     *  (used to redraw picker through a queued connection)
     */
    void sizeChanged();

protected slots:
    void update() { QOpenGLWidget::update(); }
    void redrawPicker();

protected:
    void initializeGL() override;
    void paintGL() override;
    void resizeGL(int width, int height) override;

    /*  Background items to render  */
    Axes axes;
    Background background;
    Busy busy;
    Bars bars;

    void mouseMoveEvent(QMouseEvent *event) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void wheelEvent(QWheelEvent *event) override;
    void leaveEvent(QEvent* event) override;

    Camera camera;

    struct {
        enum { RELEASED, DRAG_ROT, DRAG_PAN } state = RELEASED;
        QPoint pos;
    } mouse;

    QList<Shape*> shapes;
    QPointer<SettingsPane> pane;
    Settings settings;
    bool show_axes=true;

    /*  Framebuffer to render pick data  */
    QScopedPointer<QOpenGLFramebufferObject> pick_fbo;
};
