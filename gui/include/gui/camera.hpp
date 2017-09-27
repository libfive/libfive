#pragma once

#include <QMatrix4x4>
#include <QObject>
#include <QPropertyAnimation>

class Camera : public QObject
{
    Q_OBJECT
public:
    Camera(QSize size);

    /*
     *  Triggers an animation of the perspective member variable
     */
    void toOrthographic();
    void toPerspective();

    /*
     *  Triggers an animation to zoom to the given setting
     */
    void zoomTo(const QVector3D& min, const QVector3D& max);

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

    /*
     *  Returns aspect ratio
     */
    float getAspect() const { return size.width() / float(size.height()); }

    /*  All QPoint coordinates are in window pixels  */
    void rotateIncremental(QPoint delta);
    void panIncremental(QPoint delta);
    void zoomIncremental(float ds, QPoint c);

    float getScale() const { return scale; }

    /*  Window size  */
    QSize size;

signals:
    void changed();
    void animDone();

protected:

    float scale=0.5;
    QVector3D center={0,0,0};
    float pitch=128;
    float yaw=-59;

    float perspective=0.25;
    Q_PROPERTY(float perspective MEMBER perspective NOTIFY changed)

    QPropertyAnimation anim;
};
