#pragma once

#include <QMatrix4x4>

class Camera
{
public:
    Camera(QSize size) : size(size) { /* nothing to do here */ }

    /*
     *  Returns the projection matrix
     *  (which compensates for window aspect ratio)
     */
    QMatrix4x4 proj() const;

    /*
     *  Returns the view matrix
     *  (with rotation, scale, and center applied)
     */
    QMatrix4x4 view() const;

    /*
     *  Complete transform matrix
     */
    QMatrix4x4 M() const { return proj() * view(); }

    /*  All QPoint coordinates are in window pixels  */
    void rotateIncremental(QPoint delta);
    void panIncremental(QPoint delta);
    void zoomIncremental(float ds, QPoint c);

    /*  Resize window  */
    void resize(QSize s) { size = s; }

protected:
    /*  Window size  */
    QSize size;

    float scale=1;
    QVector3D center={0,0,0};
    float pitch=128;
    float yaw=-109;
};
