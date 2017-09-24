#include <cmath>
#include "gui/camera.hpp"

Camera::Camera(QSize size)
    : size(size), anim(this, "perspective")
{
    anim.setDuration(100);
    connect(&anim, &QPropertyAnimation::finished, this, &Camera::animDone);
}

QMatrix4x4 Camera::proj() const
{
    QMatrix4x4 m;

    //  Compress the Z axis to avoid clipping.  The exact value is arbitrary,
    //  but seems to work well for common model aspect ratios.
    const float Z_COMPRESS = 8;
    const float frac = size.width() / float(size.height());
    if (frac > 1)
    {
        m.scale(1/frac, -1, 1/Z_COMPRESS);
    }
    else
    {
        m.scale(1, -frac, 1/Z_COMPRESS);
    }
    return m;
}

QMatrix4x4 Camera::view() const
{
    QMatrix4x4 m;
    m(3, 2) = perspective;
    m.scale(scale, scale, scale);
    m.rotate(pitch, {1, 0, 0});
    m.rotate(yaw,   {0, 0, 1});
    m.translate(center);

    return m;
}


void Camera::rotateIncremental(QPoint delta)
{
    pitch += delta.y();
    yaw += delta.x();

    pitch = fmax(fmin(pitch, 180), 0);
    yaw = fmod(yaw, 360);
}

void Camera::panIncremental(QPoint delta)
{
    // Find the starting position in world coordinates
    auto inv = M().inverted();
    auto diff = inv.map({float(delta.x()/float(size.width())),
                        -float(delta.y()/float(size.height())), 0}) -
                inv.map({0, 0, 0});

    center += diff*2;
}

void Camera::zoomIncremental(float ds, QPoint c)
{
    QVector3D pt(c.x() / float(size.width()) - 0.5,
                -(c.y() / float(size.height()) - 0.5), 0);
    auto a = M().inverted().map(pt);

    scale *= pow(1.1, ds / 120.);
    center += 2 * (M().inverted().map(pt) - a);
}

void Camera::toOrthographic()
{
    anim.setStartValue(perspective);
    anim.setEndValue(0);
    anim.start();
}

void Camera::toPerspective()
{
    anim.setStartValue(perspective);
    anim.setEndValue(0.25);
    anim.start();
}

void Camera::zoomTo(const Settings& s)
{
    qDebug() << "Zooming!";
}
