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
    // We don't know which way the triangle is wound, and can't actually
    // know (because it could be parameterized, so we return the union
    // of both possible windings)
    return _union(
        intersection(intersection(
            half_plane(a, b), half_plane(b, c)), half_plane(c, a)),
        intersection(intersection(
            half_plane(a, c), half_plane(c, b)), half_plane(b, a)));
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

Tree rounded_box(TreeVec3 a, TreeVec3 b, TreeFloat r) {
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

Tree reflect_x(Tree t, TreeFloat x0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(2*x0 - x, y, z);
}

Tree reflect_y(Tree t, TreeFloat y0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, 2*y0 - y, z);
}

Tree reflect_z(Tree t, TreeFloat z0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, y, 2*z0 - z);
}

Tree reflect_xy(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(y, x, z);
}

Tree reflect_yz(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, z, y);
}

Tree reflect_xz(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(z, y, x);
}

Tree symmetric_x(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(abs(x), y, z);
}

Tree symmetric_y(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, abs(y), z);
}

Tree symmetric_z(Tree t) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, y, abs(z));
}

Tree scale_x(Tree t, TreeFloat sx, TreeFloat x0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x0 + (x - x0) / sx, y, z);
}

Tree scale_y(Tree t, TreeFloat sy, TreeFloat y0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, y0 + (y - y0) / sy, z);
}

Tree scale_z(Tree t, TreeFloat sz, TreeFloat z0) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(x, y, z0 + (z - z0) / sz);
}

Tree scale_xyz(Tree t, TreeVec3 s, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    return t.remap(
        center.x + (x - center.x) / s.x,
        center.y + (y - center.y) / s.y,
        center.z + (z - center.z) / s.z);
}

Tree rotate_x(Tree t, TreeFloat angle, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    t = move(t, TreeVec3{-center.x, -center.y, -center.z});
    return move(t.remap(x,
                        cos(angle) * y + sin(angle) * z,
                       -sin(angle) * y + cos(angle) * z), center);
}

Tree rotate_y(Tree t, TreeFloat angle, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    t = move(t, TreeVec3{-center.x, -center.y, -center.z});
    return move(t.remap(cos(angle) * x + sin(angle) * z,
                        y,
                       -sin(angle) * x + cos(angle) * z), center);
}

Tree rotate_z(Tree t, TreeFloat angle, TreeVec3 center) {
    LIBFIVE_DEFINE_XYZ();
    t = move(t, TreeVec3{-center.x, -center.y, -center.z});
    return move(t.remap(cos(angle) * x + sin(angle) * y,
                       -sin(angle) * x + cos(angle) * y, z), center);
}

Tree taper_x_y(Tree shape, TreeVec2 base, TreeFloat height,
                TreeFloat scale, TreeFloat base_scale)
{
    LIBFIVE_DEFINE_XYZ();
    const auto s = height / (scale * y + base_scale * (height - y));
    return move(
        move(shape, -TreeVec3{base.x, base.y, 0}).remap(x * s, y, z),
        TreeVec3{base.x, base.y, 0});
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

Tree shear_x_y(Tree t, TreeVec2 base, TreeFloat height, TreeFloat offset,
               TreeFloat base_offset)
{
    LIBFIVE_DEFINE_XYZ();
    const auto f = (y - base.y) / height;
    return t.remap(x - (base_offset * (1 - f)) - offset * f, y, z);
}

#define AXIS_X 1
#define AXIS_Y 2
#define AXIS_Z 4
Tree attract_repel_generic(Tree shape, TreeVec3 locus,
                           TreeFloat radius, TreeFloat exaggerate, float sign,
                           uint8_t axes)
{
    LIBFIVE_DEFINE_XYZ();
    const auto norm = sqrt(
        square((axes & AXIS_X) ? x : 0) +
        square((axes & AXIS_Y) ? y : 0) +
        square((axes & AXIS_Z) ? z : 0));
    const auto fallout = sign * exaggerate * exp(norm / radius);

    return move(
            move(shape, -locus).remap(
                x * ((axes & AXIS_X) ? fallout : 0),
                y * ((axes & AXIS_Y) ? fallout : 0),
                z * ((axes & AXIS_Z) ? fallout : 0)),
            locus);
}

Tree repel(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(
            shape, locus, radius, exaggerate, -1,
            AXIS_X | AXIS_Y | AXIS_Z);
}

Tree repel_x(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_X);
}

Tree repel_y(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_Y);
}

Tree repel_z(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_Z);
}

Tree repel_xy(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_X | AXIS_Y);
}

Tree repel_yz(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_Y | AXIS_Z);
}

Tree repel_xz(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, -1, AXIS_X | AXIS_Z);
}

Tree attract(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(
            shape, locus, radius, exaggerate, -1,
            AXIS_X | AXIS_Y | AXIS_Z);
}

Tree attract_x(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_X);
}

Tree attract_y(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_Y);
}

Tree attract_z(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_Z);
}

Tree attract_xy(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_X | AXIS_Y);
}

Tree attract_yz(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_Y | AXIS_Z);
}

Tree attract_xz(Tree shape, TreeVec3 locus, TreeFloat radius, TreeFloat exaggerate) {
    return attract_repel_generic(shape, locus, radius, exaggerate, 1, AXIS_X | AXIS_Z);
}

Tree revolve_y(Tree shape, TreeFloat x0) {
    LIBFIVE_DEFINE_XYZ();
    const auto r = sqrt(square(x) + square(y));
    const TreeVec3 center{x0, 0, 0};
    shape = move(shape, -center);
    return move(_union(shape.remap(r, y, z), shape.remap(-r, y, z)), center);
}

// This is directly ported from Scheme (hence the use of std::function), and
// I don't totally understand it - refactoring would be welcome.
Tree generic_centered_twirl_x(Tree shape, TreeFloat amount, TreeFloat radius,
                              TreeVec3 vec)
{
    LIBFIVE_DEFINE_XYZ();
    const auto norm = sqrt(square(vec.x) + square(vec.y) + square(vec.z));
    const auto ca = cos(amount * exp(-norm / radius));
    const auto sa = sin(amount * exp(-norm / radius));

    return shape.remap(x,
        ca * y + sa * z,
        ca * z - sa * y);
}

Tree centered_twirl_x(Tree shape, TreeFloat amount, TreeFloat radius) {
    LIBFIVE_DEFINE_XYZ();
    return generic_centered_twirl_x(shape, amount, radius, TreeVec3{x, y, z});
}

Tree centered_twirl_axis_x(Tree shape, TreeFloat amount, TreeFloat radius) {
    LIBFIVE_DEFINE_XYZ();
    return generic_centered_twirl_x(shape, amount, radius, TreeVec3{0, y, z});
}

Tree generic_twirl_n(Tree shape, TreeFloat amount,
                     TreeFloat radius, TreeVec3 center,
                     std::function<Tree(Tree, TreeFloat, TreeFloat)> method,
                     std::function<Tree(Tree)> remap)
{
    shape = move(shape, -center);
    shape = remap(shape);
    shape = method(shape, amount, radius);
    shape = remap(shape);
    return move(shape, center);
}

Tree generic_twirl_x(Tree shape, TreeFloat amount, TreeFloat radius,
                     TreeVec3 center,
                     std::function<Tree(Tree, TreeFloat, TreeFloat)> method)
{
    return generic_twirl_n(shape, amount, radius, center, method,
                           [](Tree t) { return t; });
}

Tree generic_twirl_y(Tree shape, TreeFloat amount, TreeFloat radius,
                     TreeVec3 center,
                     std::function<Tree(Tree, TreeFloat, TreeFloat)> method)
{
    return generic_twirl_n(shape, amount, radius, center, method, reflect_xy);
}

Tree generic_twirl_z(Tree shape, TreeFloat amount, TreeFloat radius,
                     TreeVec3 center,
                     std::function<Tree(Tree, TreeFloat, TreeFloat)> method)
{
    return generic_twirl_n(shape, amount, radius, center, method, reflect_xz);
}

Tree twirl_x(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_x(shape, amount, radius, center, centered_twirl_x);
}
Tree twirl_axis_x(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_x(shape, amount, radius, center, centered_twirl_axis_x);
}

Tree twirl_y(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_y(shape, amount, radius, center, centered_twirl_x);
}
Tree twirl_axis_y(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_y(shape, amount, radius, center, centered_twirl_axis_x);
}

Tree twirl_z(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_z(shape, amount, radius, center, centered_twirl_x);
}
Tree twirl_axis_z(Tree shape, TreeFloat amount, TreeFloat radius, TreeVec3 center) {
    return generic_twirl_z(shape, amount, radius, center, centered_twirl_axis_x);
}
