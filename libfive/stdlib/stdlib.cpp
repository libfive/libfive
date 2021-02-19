/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "stdlib.h"

using namespace libfive;

#define LIBFIVE_DEFINE_XYZ() const auto x = Tree::X(); (void)x; \
                             const auto y = Tree::Y(); (void)y; \
                             const auto z = Tree::Z(); (void)z; ;

////////////////////////////////////////////////////////////////////////////////
// Operator overloads for C vecs
vec2 operator+(const vec2& a, const vec2& b) {
    return vec2{a.x + b.x, a.y + b.y};
}
vec2 operator-(const vec2& a, const vec2& b) {
    return vec2{a.x - b.x, a.y - b.y};
}
vec2 operator*(const vec2& a, const float& b) {
    return vec2{a.x * b, a.y * b};
}
vec2 operator/(const vec2& a, const float& b) {
    return vec2{a.x / b, a.y / b};
}
vec3 operator+(const vec3& a, const vec3& b) {
    return vec3{a.x + b.x, a.y + b.y, a.z + b.z};
}
vec3 operator-(const vec3& a, const vec3& b) {
    return vec3{a.x - b.x, a.y - b.y, a.z - b.z};
}
vec3 operator-(const vec3& a) {
    return vec3{-a.x, -a.y, -a.z};
}
vec3 operator*(const vec3& a, const float& b) {
    return vec3{a.x * b, a.y * b, a.z * b};
}
vec3 operator/(const vec3& a, const float& b) {
    return vec3{a.x / b, a.y / b, a.z / b};
}
////////////////////////////////////////////////////////////////////////////////
// csg
Tree _union(Tree a, Tree b) {
    return min(a, b);
}
LIBFIVE_STDLIB _union(libfive_tree a, libfive_tree b) {
    return _union(Tree(a), Tree(b)).release();
}

Tree intersection(Tree a, Tree b) {
    return max(a, b);
}
LIBFIVE_STDLIB intersection(libfive_tree a, libfive_tree b) {
    return intersection(Tree(a), Tree(b)).release();
}

Tree inverse(Tree a) {
    return -a;
}
LIBFIVE_STDLIB inverse(libfive_tree a) {
    return inverse(Tree(a)).release();
}

Tree difference(Tree a, Tree b) {
    return intersection(a, inverse(b));
}
LIBFIVE_STDLIB difference(libfive_tree a, libfive_tree b) {
    return difference(Tree(a), Tree(b)).release();
}

Tree offset(Tree a, float off) {
    return a - off;
}
LIBFIVE_STDLIB offset(libfive_tree a, float off) {
    return offset(Tree(a), off).release();
}

Tree clearance(Tree a, Tree b, float o) {
    return difference(a, offset(b, o));
}
LIBFIVE_STDLIB clearance(libfive_tree a, libfive_tree b, float o) {
    return clearance(Tree(a), Tree(b), o).release();
}

Tree shell(Tree a, float o) {
    return clearance(a, a, o);
}
LIBFIVE_STDLIB shell(libfive_tree a, float o) {
    return shell(Tree(a), o).release();
}

Tree blend_expt(Tree a, Tree b, float m) {
    return -log(exp(-m * a) + exp(-m * b)) / m;
}
LIBFIVE_STDLIB blend_expt(libfive_tree a, libfive_tree b, float m) {
    return blend_expt(Tree(a), Tree(b), m).release();
}

Tree blend_expt_unit(Tree a, Tree b, float m) {
    return blend_expt(a, b, 2.75 / pow(m, 2));
}
LIBFIVE_STDLIB blend_expt_unit(libfive_tree a, libfive_tree b, float m) {
    return blend_expt_unit(Tree(a), Tree(b), m).release();
}

Tree blend_rough(Tree a, Tree b, float m) {
    auto c = sqrt(abs(a)) + sqrt(abs(b)) - m;
    return _union(a, _union(b, c));
}
LIBFIVE_STDLIB blend_rough(libfive_tree a, libfive_tree b, float m) {
    return blend_rough(Tree(a), Tree(b), m).release();
}

Tree blend_difference(Tree a, Tree b, float m, float o) {
    return inverse(blend_expt_unit(inverse(a), offset(b, o), m));
}
LIBFIVE_STDLIB blend_difference(libfive_tree a, libfive_tree b, float m, float o) {
    return blend_difference(Tree(a), Tree(b), m, o).release();
}

Tree morph(Tree a, Tree b, float m) {
    return a * (1 - m) + b * m;
}
LIBFIVE_STDLIB morph(libfive_tree a, libfive_tree b, float m) {
    return morph(Tree(a), Tree(b), m).release();
}

LIBFIVE_STDLIB loft(libfive_tree a, libfive_tree b, float zmin, float zmax) {
    LIBFIVE_DEFINE_XYZ();
    return max(z - zmax, max(zmin - z,
        ((z - zmin) * Tree(b) + (zmax - z) * Tree(a))
            / (zmax - zmin))).release();
}

LIBFIVE_STDLIB loft_between(libfive_tree a, libfive_tree b, vec3 lower, vec3 upper) {
    LIBFIVE_DEFINE_XYZ();

    const auto f = (z - lower.z) / (upper.z - lower.z);
    const auto g = (upper.z - z) / (upper.z - lower.z);

    auto a_ = Tree(a).remap(
            x + (f * (lower.x - upper.x)),
            y + (f * (lower.y - upper.y)),
            z);
    auto b_ = Tree(b).remap(
            x + (g * (upper.x - lower.x)),
            y + (g * (upper.y - lower.y)),
            z);
    return loft(a_.release(), b_.release(), lower.z, upper.z);
}

////////////////////////////////////////////////////////////////////////////////
// shapes
LIBFIVE_STDLIB circle(float r, vec2 center) {
    LIBFIVE_DEFINE_XYZ();
    auto c = sqrt(x * x + y * y) - r;
    return move(c.release(), vec3{center.x, center.y, 0});
}

LIBFIVE_STDLIB ring(float ro, float ri, vec2 center) {
    return difference(circle(ro, center), circle(ri, center));
}

LIBFIVE_STDLIB polygon(float r, int n, vec2 center) {
    LIBFIVE_DEFINE_XYZ();
    r = r * cos(M_PI / n);
    const auto half = (y - r).release();
    auto out = half;
    for (int i=1; i < n; ++i) {
        out = intersection(out, rotate_z(half, 2 * M_PI * i / n, vec3{0, 0, 0}));
    }
    return move(out, vec3{center.x, center.y, 0});
}

LIBFIVE_STDLIB rectangle(vec2 a, vec2 b) {
    LIBFIVE_DEFINE_XYZ();
    return max(
        max(a.x - x, x - b.x),
        max(a.y - y, y - b.y)).release();
}

LIBFIVE_STDLIB rounded_rectangle(vec2 a, vec2 b, float r) {
    return _union(
        _union(rectangle(vec2{a.x, a.y + r}, vec2{b.x, b.y - r}),
               rectangle(vec2{a.x + r, a.y}, vec2{b.x - r, b.y})),
        _union(
            _union(circle(r, vec2{a.x + r, a.y + r}),
                   circle(r, vec2{b.x - r, b.y - r})),
            _union(circle(r, vec2{a.x + r, b.y - r}),
                   circle(r, vec2{b.x - r, a.y + r}))));
}

LIBFIVE_STDLIB rectangle_exact(vec2 a, vec2 b) {
    vec2 size = b - a;
    vec2 center = (a + b) / 2;
    return rectangle_centered_exact(size, center);
}

LIBFIVE_STDLIB rectangle_centered_exact(vec2 size, vec2 center) {
    LIBFIVE_DEFINE_XYZ();
    const auto dx = abs(x) - size.x/2;
    const auto dy = abs(y) - size.y/2;
    return move(
        (min(max(dx, dy), 0) +
         sqrt(square(max(dx, 0)) + square(max(dy, 0)))).release(),
        vec3{center.x, center.y, 0});
}

libfive_tree half_plane(vec2 a, vec2 b) {
    LIBFIVE_DEFINE_XYZ();
    return ((b.y - a.y) * (x - a.x) - (b.x - a.x) * (y - a.y)).release();
}

LIBFIVE_STDLIB triangle(vec2 a, vec2 b, vec2 c) {
    LIBFIVE_DEFINE_XYZ();
    const auto vec_ab = Eigen::Vector3f(b.x - a.x, b.y - a.y, 0);
    const auto vec_ac = Eigen::Vector3f(c.x - a.x, c.y - a.y, 0);
    if (vec_ab.cross(vec_ac).z() > 0.0f) {
        return intersection(intersection(
            half_plane(a, b), half_plane(b, c)), half_plane(c, a));
    } else {
        return intersection(intersection(
            half_plane(a, c), half_plane(c, b)), half_plane(b, a));
    }
}

//------------------------------------------------------------------------------
LIBFIVE_STDLIB box_mitered(vec3 a, vec3 b) {
    return extrude_z(rectangle(vec2{a.x, a.y}, vec2{b.x, b.y}), a.z, b.z);
}

LIBFIVE_STDLIB box_mitered_centered(vec3 size, vec3 center) {
    return box_mitered(center - size / 2, center + size / 2);
}

LIBFIVE_STDLIB box_exact_centered(vec3 size, vec3 center) {
    LIBFIVE_DEFINE_XYZ();
    const auto dx = abs(x - center.x) - (size.x / 2);
    const auto dy = abs(y - center.y) - (size.y / 2);
    const auto dz = abs(z - center.z) - (size.z / 2);
    return (min(0, max(dx, max(dy, dz))) +
            sqrt(square(max(dx, 0)) + square(max(dy, 0)) + square(max(dz, 0))))
        .release();
}

LIBFIVE_STDLIB box_exact(vec3 a, vec3 b) {
    return box_exact_centered(b - a, (a + b) / 2);
}

LIBFIVE_STDLIB box_rounded(vec3 a, vec3 b, float r) {
    const auto d = b - a;
    r *= fminf(d.x, fminf(d.y, d.z)) / 2;
    vec3 v{r, r, r};
    return offset(box_exact(a + v, b - v), r);
}

LIBFIVE_STDLIB sphere(float r, vec3 center) {
    LIBFIVE_DEFINE_XYZ();
    return move((sqrt(square(x) + square(y) + square(z)) - r).release(),
            center);
}

LIBFIVE_STDLIB half_space(vec3 norm, vec3 point) {
    LIBFIVE_DEFINE_XYZ();
    // dot(pos - point, norm)
    return ((x - point.x) * norm.x +
            (y - point.y) * norm.y +
            (z - point.z) * norm.z).release();
}

LIBFIVE_STDLIB cylinder_z(float r, float h, vec3 base) {
    return extrude_z(circle(r, vec2{base.x, base.y}), base.z, base.z + h);
}

LIBFIVE_STDLIB cone_ang_z(float angle, float height, vec3 base) {
    LIBFIVE_DEFINE_XYZ();
    return move(max(-z, cos(angle) * sqrt(square(x) + square(y))
                      + sin(angle) * z - height).release(),
                base);
}

LIBFIVE_STDLIB pyramid_z(vec2 a, vec2 b, float zmin, float height) {
    // TODO: make this an intersection of planes instead, to avoid singularity
    return taper_xy_z(
        extrude_z(rectangle(a, b), zmin, zmin + height),
        vec3{ (a.x + b.x) / 2, (a.y + b.y) / 2, zmin},
        height, 0, 1);
}

LIBFIVE_STDLIB torus_z(float ro, float ri, vec3 center) {
    LIBFIVE_DEFINE_XYZ();
    return move(
        (sqrt(square(ro - sqrt(square(x) + square(y)))
            + square(z)) - ri).release(),
        center);
}

LIBFIVE_STDLIB gyroid(vec3 period, float thickness) {
    LIBFIVE_DEFINE_XYZ();
    const auto tau = 2 * M_PI;
    return shell(
        (sin(x * period.x / tau) * cos(y * period.y / tau) +
         sin(y * period.y / tau) * cos(z * period.z / tau) +
         sin(z * period.z / tau) * cos(x * period.x / tau)).release(),
        -thickness);
}

//------------------------------------------------------------------------------

LIBFIVE_STDLIB array_x(libfive_tree shape, int nx, float dx) {
    for (int i=1; i < nx; ++i) {
        shape = _union(shape, move(shape, vec3{dx * i, 0, 0}));
    }
    return shape;
}

LIBFIVE_STDLIB array_xy(libfive_tree shape, int nx, int ny, vec2 delta) {
    shape = array_x(shape, nx, delta.x);
    for (int i=1; i < ny; ++i) {
        shape = _union(shape, move(shape, vec3{0, delta.y * i, 0}));
    }
    return shape;
}

LIBFIVE_STDLIB array_xyz(libfive_tree shape, int nx, int ny, int nz,
                         vec3 delta) {
    shape = array_xy(shape, nx, ny, vec2{delta.x, delta.y});
    for (int i=1; i < nz; ++i) {
        shape = _union(shape, move(shape, vec3{0, 0, delta.y * i}));
    }
    return shape;
}

LIBFIVE_STDLIB array_polar_z(libfive_tree shape, int n, vec2 center) {
    const float a = 2 * M_PI / n;
    vec3 c{center.x, center.y, 0};
    for (int i=0; i < n; ++i) {
        shape = _union(shape, rotate_z(shape, i * a, c));
    }
    return shape;
}

LIBFIVE_STDLIB extrude_z(libfive_tree t, float zmin, float zmax) {
    LIBFIVE_DEFINE_XYZ();
    return max(Tree(t), max(zmin - z, z - zmax)).release();
}


////////////////////////////////////////////////////////////////////////////////
// transforms
LIBFIVE_STDLIB move(libfive_tree t_, vec3 offset) {
    LIBFIVE_DEFINE_XYZ();
    auto t = Tree(t_);
    return t.remap(x - offset.x, y - offset.y, z - offset.z).release();
}

LIBFIVE_STDLIB rotate_z(libfive_tree t_, float angle, vec3 center) {
    LIBFIVE_DEFINE_XYZ();
    const auto t = Tree(move(t_, vec3{-center.x, -center.y, -center.z}));
    return move(t.remap(cos(angle) * x + sin(angle) * y,
                       -sin(angle) * x + cos(angle) * y, z).release(), center);
}

LIBFIVE_STDLIB taper_xy_z(libfive_tree shape, vec3 base, float height,
                          float scale, float base_scale)
{
    LIBFIVE_DEFINE_XYZ();
    const auto s = height / (scale * z + base_scale * (height - z));
    return move(
        Tree(move(shape, -base)).remap(x * s, y * s, z).release(),
        base);
}
