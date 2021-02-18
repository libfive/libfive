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
// csg
LIBFIVE_STDLIB _union(libfive_tree a, libfive_tree b) {
    return min(Tree(a), Tree(b)).release();
}

LIBFIVE_STDLIB intersection(libfive_tree a, libfive_tree b) {
    return max(Tree(a), Tree(b)).release();
}

LIBFIVE_STDLIB inverse(libfive_tree a) {
    return (-Tree(a)).release();
}

LIBFIVE_STDLIB difference(libfive_tree a, libfive_tree b) {
    return intersection(a, inverse(b));
}

LIBFIVE_STDLIB offset(libfive_tree a, float offset) {
    return (Tree(a) - offset).release();
}

LIBFIVE_STDLIB clearance(libfive_tree a, libfive_tree b, float o) {
    return difference(a, offset(b, o));
}

LIBFIVE_STDLIB shell(libfive_tree a, float o) {
    return clearance(a, a, o);
}

LIBFIVE_STDLIB blend_expt(libfive_tree a, libfive_tree b, float m) {
    return (-log(exp(-m * Tree(a)) + exp(-m * Tree(b))) / m).release();
}

LIBFIVE_STDLIB blend_expt_unit(libfive_tree a, libfive_tree b, float m) {
    return blend_expt(a, b, 2.75 / pow(m, 2));
}

LIBFIVE_STDLIB blend_rough(libfive_tree a, libfive_tree b, float m) {
    return _union(a, _union(b,
        (sqrt(abs(Tree(a))) + sqrt(abs(Tree(b))) - m).release()));
}

LIBFIVE_STDLIB blend(libfive_tree a, libfive_tree b, float m) {
    return blend_expt_unit(a, b, m);
}

LIBFIVE_STDLIB blend_difference(libfive_tree a, libfive_tree b, float m, float o) {
    return inverse(blend(inverse(a), offset(b, o), m));
}

LIBFIVE_STDLIB morph(libfive_tree a, libfive_tree b, float m) {
    return (Tree(a) * (1 - m) + Tree(b) * m).release();
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
    vec2 size = {b.x - a.x, b.y - a.y};
    vec2 center = {(a.x + b.x) / 2, (a.y + b.y) / 2};
    return rectangle_centered_exact(size, center);
}

LIBFIVE_STDLIB rectangle_centered_exact(vec2 size, vec2 center) {
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
