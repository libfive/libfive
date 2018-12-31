/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/tree/tree.hpp"

#include "shapes.hpp"

using namespace Kernel;

Tree rectangle(double xmin, double xmax, double ymin, double ymax,
               Eigen::Matrix4d M)
{
    auto x = M(0,0)*Tree::X() + M(0,1)*Tree::Y() + M(0,2)*Tree::Z() + M(0,3);
    auto y = M(1,0)*Tree::X() + M(1,1)*Tree::Y() + M(1,2)*Tree::Z() + M(1,3);

    return max(max(xmin - x, x - xmax), max(ymin - y, y - ymax));
}

Tree rotate2d(Tree t, double angle)
{
    return t.remap( cos(angle) * Tree::X() + sin(angle) * Tree::Y(),
                   -sin(angle) * Tree::X() + cos(angle) * Tree::Y(),
                   Tree::Z());
}

Tree move(Tree t, Eigen::Vector3d m)
{
    return t.remap(Tree::X() - m.x(), Tree::Y() - m.y(), Tree::Z() - m.z());
}

Tree recurse(double x, double y, double scale, Eigen::Matrix4d M, int i)
{
    auto base = rectangle(x - scale/2, x + scale/2,
                          y - scale/2, y + scale/2, M);

    if (i == 0)
    {
        return base;
    }
    else
    {
        auto j = i - 1;
        auto t = scale / 3;

        return min(base,
               min(recurse(x + scale, y, t, M, j),
               min(recurse(x - scale, y, t, M, j),
               min(recurse(x, y + scale, t, M, j),
               min(recurse(x, y - scale, t, M, j),
               min(recurse(x + scale, y + scale, t, M, j),
               min(recurse(x + scale, y - scale, t, M, j),
               min(recurse(x - scale, y + scale, t, M, j),
                   recurse(x - scale, y - scale, t, M, j)
               ))))))));
    }
}

Tree menger(int i)
{
    Eigen::Matrix3d m = Eigen::Matrix3d::Identity();
    Eigen::Matrix4d M = Eigen::Matrix4d::Zero();
    M.block<3,3>(0,0) = m;
    Tree a = recurse(0, 0, 1, M, i);

    m = Eigen::AngleAxisd(double(M_PI/2), Eigen::Vector3d::UnitX());
    M.block<3,3>(0,0) = m;
    Tree b = recurse(0, 0, 1, M, i);

    m = Eigen::AngleAxisd(double(M_PI/2), Eigen::Vector3d::UnitY());
    M.block<3,3>(0,0) = m;
    Tree c = recurse(0, 0, 1, M, i);

    auto cube = max(max(
                    max(-(Tree::X() + 1.5),
                          Tree::X() - 1.5),
                    max(-(Tree::Y() + 1.5),
                          Tree::Y() - 1.5)),
                    max(-(Tree::Z() + 1.5),
                          Tree::Z() - 1.5));

    auto cutout = -min(min(a, b), c);
    return max(cube, cutout);
}

Tree circle(double r, Eigen::Vector2d center)
{
    return sqrt(square(Tree::X() - center.x()) + square(Tree::Y() - center.y())) - r;
}

Tree sphere(double r, Eigen::Vector3d center)
{
    return sqrt(square(Tree::X() - center.x()) +
                square(Tree::Y() - center.y()) +
                square(Tree::Z() - center.z())) - r;
}

Tree box(const Eigen::Vector3d& lower, const Eigen::Vector3d& upper)
{
    return max(max(
               max(lower.x() - Tree::X(),
                   Tree::X() - upper.x()),
               max(lower.y() - Tree::Y(),
                   Tree::Y() - upper.y())),
               max(lower.z() - Tree::Z(),
                   Tree::Z() - upper.z()));
}

Tree shell(Tree t, double offset)
{
    return max(t, t - offset);
}

Tree blend(Tree a, Tree b, double r)
{
    auto vc0 = r - a;
    auto vc1 = r - b;

    auto u0 = max(vc0, 0.f);
    auto u1 = max(vc1, 0.f);

    auto len = sqrt(square(u0) + square(u1));

    return max(r, min(a, b)) - len;
}

Tree cylinder(double r, double h, Eigen::Vector3d base)
{
    return extrude(move(circle(r), base), base.z(), base.z() + h);
}

Tree extrude(Tree a, double lower, double upper)
{
    return max(a, max(lower - Tree::Z(), Tree::Z() - upper));
}

Tree sphereGyroid()
{
    auto scale = 0.5f;
    auto thickness = 0.5;

    auto gyroidSrf =
        sin(Kernel::Tree::X() / scale) * cos(Kernel::Tree::Y() / scale) +
        sin(Kernel::Tree::Y() / scale) * cos(Kernel::Tree::Z() / scale) +
        sin(Kernel::Tree::Z() / scale) * cos(Kernel::Tree::X() / scale);

    auto gyroid = shell(gyroidSrf, thickness);
    auto sphere1 = sphere(3.0f, { 0.f,0.f,0.f });

    auto sphereGyroid = max(sphere1, gyroid);
    sphereGyroid = min(sphereGyroid,
                     min(sphereGyroid,
                     (sqrt(abs(sphereGyroid)) + sqrt(abs( sphereGyroid ))) - .5));

    return sphereGyroid;
}
