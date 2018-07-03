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
#include <cmath>

#include "studio/camera.hpp"

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
    const float frac = size.width() / float(size.height());
    const float Z_COMPRESS = 8;
    if (frac > 1)
    {
        m.scale(1/frac, -1, 1 / Z_COMPRESS);
    }
    else
    {
        m.scale(1, -frac, 1 / Z_COMPRESS);
    }

    m(3, 2) = perspective;
    return m;
}

QMatrix4x4 Camera::view() const
{
    QMatrix4x4 m;
    m.scale(scale, scale, scale);
    m.rotate(pitch, {1, 0, 0});
    m.rotate(yaw,   {0, 0, 1});
    m.rotate(axis);
    m.translate(center);

    return m;
}

QMatrix4x4 Camera::M() const
{
    return proj() * view();
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

void Camera::animateAxis(QQuaternion end)
{
    auto a = new QVariantAnimation(this);
    a->setStartValue(0);
    a->setEndValue(1000);
    a->setDuration(200);
    a->setEasingCurve(QEasingCurve::InOutSine);

    const QQuaternion start = axis;

    connect(a, &QVariantAnimation::valueChanged,
            this, [=](QVariant _v){
                auto v = _v.toFloat() / a->endValue().toFloat();
                axis = QQuaternion::slerp(start, end, v);
                emit(changed());
            });
    connect(a, &QPropertyAnimation::finished, this, &Camera::animDone);
    a->start(a->DeleteWhenStopped);
}

void Camera::toTurnZ()
{
    animateAxis(QQuaternion::fromDirection({0, 0, 1}, {0, 1, 0}));
}

void Camera::toTurnY()
{
    animateAxis(QQuaternion::fromDirection({0, 1, 0}, {1, 0, 0}));
}

void Camera::zoomTo(const QVector3D& min, const QVector3D& max)
{
    QVector3D center_start = center;
    QVector3D center_end = (min + max) / -2;

    float scale_start = scale;
    float scale_end = 2 / (max - min).length();

    auto a = new QVariantAnimation(this);
    a->setStartValue(0);
    a->setEndValue(1000);
    a->setDuration(100);
    a->setEasingCurve(QEasingCurve::InOutSine);

    connect(a, &QVariantAnimation::valueChanged,
            this, [=](QVariant _v){
                auto v = _v.toFloat() / a->endValue().toFloat();
                scale = scale_end * v + scale_start * (1 - v);
                v = pow(v, 6);
                center = center_end * v + center_start * (1 - v);
                emit(changed());
            });
    connect(a, &QPropertyAnimation::finished, this, &Camera::animDone);
    a->start(a->DeleteWhenStopped);
}
