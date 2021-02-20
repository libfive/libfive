/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "stdlib_impl.hpp"

using namespace libfive;

#define LIBFIVE_DEFINE_XYZ() const auto x = Tree::X(); (void)x; \
                             const auto y = Tree::Y(); (void)y; \
                             const auto z = Tree::Z(); (void)z; ;
////////////////////////////////////////////////////////////////////////////////
// Operator overloads for C vecs
TreeVec2 operator+(const TreeVec2& a, const TreeVec2& b) {
    return TreeVec2{a.x + b.x, a.y + b.y};
}
TreeVec2 operator-(const TreeVec2& a, const TreeVec2& b) {
    return TreeVec2{a.x - b.x, a.y - b.y};
}
TreeVec2 operator*(const TreeVec2& a, const TreeFloat& b) {
    return TreeVec2{a.x * b, a.y * b};
}
TreeVec2 operator/(const TreeVec2& a, const TreeFloat& b) {
    return TreeVec2{a.x / b, a.y / b};
}
TreeVec3 operator+(const TreeVec3& a, const TreeVec3& b) {
    return TreeVec3{a.x + b.x, a.y + b.y, a.z + b.z};
}
TreeVec3 operator-(const TreeVec3& a, const TreeVec3& b) {
    return TreeVec3{a.x - b.x, a.y - b.y, a.z - b.z};
}
TreeVec3 operator-(const TreeVec3& a) {
    return TreeVec3{-a.x, -a.y, -a.z};
}
TreeVec3 operator*(const TreeVec3& a, const TreeFloat& b) {
    return TreeVec3{a.x * b, a.y * b, a.z * b};
}
TreeVec3 operator/(const TreeVec3& a, const TreeFloat& b) {
    return TreeVec3{a.x / b, a.y / b, a.z / b};
}
////////////////////////////////////////////////////////////////////////////////
// csg
Tree _union(Tree a, Tree b) {
    return min(a, b);
}

Tree intersection(Tree a, Tree b) {
    return max(a, b);
}

Tree inverse(Tree a) {
    return -a;
}

Tree difference(Tree a, Tree b) {
    return intersection(a, inverse(b));
}

Tree offset(Tree a, TreeFloat off) {
    return a - off;
}

Tree clearance(Tree a, Tree b, TreeFloat o) {
    return difference(a, offset(b, o));
}

Tree shell(Tree a, TreeFloat o) {
    return clearance(a, a, o);
}

Tree blend_expt(Tree a, Tree b, TreeFloat m) {
    return -log(exp(-m * a) + exp(-m * b)) / m;
}

Tree blend_expt_unit(Tree a, Tree b, TreeFloat m) {
    return blend_expt(a, b, 2.75 / pow(m, 2));
}

Tree blend_rough(Tree a, Tree b, TreeFloat m) {
    auto c = sqrt(abs(a)) + sqrt(abs(b)) - m;
    return _union(a, _union(b, c));
}

Tree blend_difference(Tree a, Tree b, TreeFloat m, TreeFloat o) {
    return inverse(blend_expt_unit(inverse(a), offset(b, o), m));
}

Tree morph(Tree a, Tree b, TreeFloat m) {
    return a * (1 - m) + b * m;
}

Tree loft(Tree a, Tree b, TreeFloat zmin, TreeFloat zmax) {
    LIBFIVE_DEFINE_XYZ();
    return max(z - zmax, max(zmin - z,
        ((z - zmin) * b + (zmax - z) * a) / (zmax - zmin)));
}

Tree loft_between(Tree a, Tree b, TreeVec3 lower, TreeVec3 upper) {
    LIBFIVE_DEFINE_XYZ();

    const auto f = (z - lower.z) / (upper.z - lower.z);
    const auto g = (upper.z - z) / (upper.z - lower.z);

    a = a.remap(
            x + (f * (lower.x - upper.x)),
            y + (f * (lower.y - upper.y)),
            z);
    b = b.remap(
            x + (g * (upper.x - lower.x)),
            y + (g * (upper.y - lower.y)),
            z);
    return loft(a, b, lower.z, upper.z);
}

////////////////////////////////////////////////////////////////////////////////
// shapes
Tree circle(TreeFloat r, TreeVec2 center) {
    LIBFIVE_DEFINE_XYZ();
    auto c = sqrt(x * x + y * y) - r;
    return move(c, TreeVec3{center.x, center.y, 0});
}

Tree ring(TreeFloat ro, TreeFloat ri, TreeVec2 center) {
    return difference(circle(ro, center), circle(ri, center));
}

Tree polygon(TreeFloat r, int n, TreeVec2 center) {
    LIBFIVE_DEFINE_XYZ();
    r = r * cos(M_PI / n);
    const auto half = y - r;
    auto out = half;
    for (int i=1; i < n; ++i) {
        out = intersection(out, rotate_z(half, 2 * M_PI * i / n, TreeVec3{0, 0, 0}));
    }
    return move(out, TreeVec3{center.x, center.y, 0});
}

Tree rectangle(TreeVec2 a, TreeVec2 b) {
    LIBFIVE_DEFINE_XYZ();
    return max(
        max(a.x - x, x - b.x),
        max(a.y - y, y - b.y));
}

Tree rounded_rectangle(TreeVec2 a, TreeVec2 b, TreeFloat r) {
    return _union(
        _union(rectangle(TreeVec2{a.x, a.y + r}, TreeVec2{b.x, b.y - r}),
               rectangle(TreeVec2{a.x + r, a.y}, TreeVec2{b.x - r, b.y})),
        _union(
            _union(circle(r, TreeVec2{a.x + r, a.y + r}),
                   circle(r, TreeVec2{b.x - r, b.y - r})),
            _union(circle(r, TreeVec2{a.x + r, b.y - r}),
                   circle(r, TreeVec2{b.x - r, a.y + r}))));
}

Tree rectangle_exact(TreeVec2 a, TreeVec2 b) {
    TreeVec2 size = b - a;
    TreeVec2 center = (a + b) / 2;
    return rectangle_centered_exact(size, center);
}

Tree rectangle_centered_exact(TreeVec2 size, TreeVec2 center) {
    LIBFIVE_DEFINE_XYZ();
    const auto dx = abs(x) - size.x/2;
    const auto dy = abs(y) - size.y/2;
    return move(
        min(max(dx, dy), 0) +
        sqrt(square(max(dx, 0)) + square(max(dy, 0))),
        TreeVec3{center.x, center.y, 0});
}

Tree half_plane(TreeVec2 a, TreeVec2 b) {
    LIBFIVE_DEFINE_XYZ();
    return (b.y - a.y) * (x - a.x) - (b.x - a.x) * (y - a.y);
}

Tree triangle(TreeVec2 a, TreeVec2 b, TreeVec2 c) {
    LIBFIVE_DEFINE_XYZ();
    /*
    const auto vec_ab = Eigen::Vector3f(b.x - a.x, b.y - a.y, 0);
    const auto vec_ac = Eigen::Vector3f(c.x - a.x, c.y - a.y, 0);
    if (vec_ab.cross(vec_ac).z() > 0.0f) {
        return intersection(intersection(
            half_plane(a, b), half_plane(b, c)), half_plane(c, a));
    } else {
        return intersection(intersection(
            half_plane(a, c), half_plane(c, b)), half_plane(b, a));
    }
    */
    (void)a;
    (void)b;
    (void)c;
    return Tree::X(); // TODO
}

//------------------------------------------------------------------------------
Tree box_mitered(TreeVec3 a, TreeVec3 b) {
    return extrude_z(rectangle(TreeVec2{a.x, a.y}, TreeVec2{b.x, b.y}), a.z, b.z);
}

Tree box_mitered_centered(TreeVec3 size, TreeVec3 center) {
    return box_mitered(center - size / 2, center + size / 2);
}

Tree box_exact_centered(TreeVec3 size, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    const auto dx = abs(x - center.x) - (size.x / 2);
    const auto dy = abs(y - center.y) - (size.y / 2);
    const auto dz = abs(z - center.z) - (size.z / 2);
    return min(0, max(dx, max(dy, dz))) + sqrt(square(max(dx, 0)) +
                                               square(max(dy, 0)) +
                                               square(max(dz, 0)));
}

Tree box_exact(TreeVec3 a, TreeVec3 b) {
    return box_exact_centered(b - a, (a + b) / 2);
}

Tree box_rounded(TreeVec3 a, TreeVec3 b, TreeFloat r) {
    const auto d = b - a;
    r = r * min(d.x, min(d.y, d.z)) / 2;
    TreeVec3 v{r, r, r};
    return offset(box_exact(a + v, b - v), r);
}

Tree sphere(TreeFloat r, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    return move(sqrt(square(x) + square(y) + square(z)) - r, center);
}

Tree half_space(TreeVec3 norm, TreeVec3 point) {
    LIBFIVE_DEFINE_XYZ();
    // dot(pos - point, norm)
    return (x - point.x) * norm.x +
           (y - point.y) * norm.y +
           (z - point.z) * norm.z;
}

Tree cylinder_z(TreeFloat r, TreeFloat h, TreeVec3 base) {
    return extrude_z(circle(r, TreeVec2{base.x, base.y}), base.z, base.z + h);
}

Tree cone_ang_z(TreeFloat angle, TreeFloat height, TreeVec3 base) {
    LIBFIVE_DEFINE_XYZ();
    return move(max(-z, cos(angle) * sqrt(square(x) + square(y))
                      + sin(angle) * z - height),
                base);
}

Tree cone_z(TreeFloat radius, TreeFloat height, TreeVec3 base) {
    return cone_ang_z(atan2(radius, height), height, base);
}

Tree pyramid_z(TreeVec2 a, TreeVec2 b, TreeFloat zmin, TreeFloat height) {
    // TODO: make this an intersection of planes instead, to avoid singularity
    return taper_xy_z(
        extrude_z(rectangle(a, b), zmin, zmin + height),
        TreeVec3{ (a.x + b.x) / 2, (a.y + b.y) / 2, zmin},
        height, 0, 1);
}

Tree torus_z(TreeFloat ro, TreeFloat ri, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    return move(
        sqrt(square(ro - sqrt(square(x) + square(y)))
           + square(z)) - ri,
        center);
}

Tree gyroid(TreeVec3 period, TreeFloat thickness) {
    LIBFIVE_DEFINE_XYZ();
    const auto tau = 2 * M_PI;
    return shell(
        sin(x * period.x / tau) * cos(y * period.y / tau) +
        sin(y * period.y / tau) * cos(z * period.z / tau) +
        sin(z * period.z / tau) * cos(x * period.x / tau),
        -thickness);
}

//------------------------------------------------------------------------------

Tree array_x(Tree shape, int nx, TreeFloat dx) {
    for (int i=1; i < nx; ++i) {
        shape = _union(shape, move(shape, TreeVec3{dx * i, 0, 0}));
    }
    return shape;
}

Tree array_xy(Tree shape, int nx, int ny, TreeVec2 delta) {
    shape = array_x(shape, nx, delta.x);
    for (int i=1; i < ny; ++i) {
        shape = _union(shape, move(shape, TreeVec3{0, delta.y * i, 0}));
    }
    return shape;
}

Tree array_xyz(Tree shape, int nx, int ny, int nz,
                         TreeVec3 delta) {
    shape = array_xy(shape, nx, ny, TreeVec2{delta.x, delta.y});
    for (int i=1; i < nz; ++i) {
        shape = _union(shape, move(shape, TreeVec3{0, 0, delta.y * i}));
    }
    return shape;
}

Tree array_polar_z(Tree shape, int n, TreeVec2 center) {
    const float a = 2 * M_PI / n;
    TreeVec3 c{center.x, center.y, 0};
    for (int i=0; i < n; ++i) {
        shape = _union(shape, rotate_z(shape, i * a, c));
    }
    return shape;
}

Tree extrude_z(Tree t, TreeFloat zmin, TreeFloat zmax) {
    LIBFIVE_DEFINE_XYZ();
    return max(t, max(zmin - z, z - zmax));
}


////////////////////////////////////////////////////////////////////////////////
// transforms
Tree move(Tree t, TreeVec3 offset) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x - offset.x, y - offset.y, z - offset.z);
}

Tree rotate_z(Tree t, TreeFloat angle, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    t = move(t, TreeVec3{-center.x, -center.y, -center.z});
    return move(t.remap(cos(angle) * x + sin(angle) * y,
                       -sin(angle) * x + cos(angle) * y, z), center);
}

Tree taper_xy_z(Tree shape, TreeVec3 base, TreeFloat height,
                TreeFloat scale, TreeFloat base_scale)
{
    LIBFIVE_DEFINE_XYZ();
    const auto s = height / (scale * z + base_scale * (height - z));
    return move(
        move(shape, -base).remap(x * s, y * s, z),
        base);
}
