/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2021  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "stdlib.h"

using namespace libfive;

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
    const auto z = Tree::Z();
    return max(z - zmax, max(zmin - z,
        ((z - zmin) * Tree(b) + (zmax - z) * Tree(a))
            / (zmax - zmin))).release();
}

LIBFIVE_STDLIB loft_between(libfive_tree a, libfive_tree b, vec3 lower, vec3 upper) {
    const auto x = Tree::X();
    const auto y = Tree::Y();
    const auto z = Tree::Z();

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

LIBFIVE_STDLIB circle(float r, vec2 center) {
    auto x = Tree::X();
    auto y = Tree::Y();
    auto c = sqrt(x * x + y * y) - r;
    return move(c.release(), vec3{center.x, center.y, 0});
}

LIBFIVE_STDLIB move(libfive_tree t_, vec3 offset) {
    auto t = Tree(t_);
    return t.remap(Tree::X() - offset.x,
                   Tree::Y() - offset.y,
                   Tree::Z() - offset.z).release();
}
