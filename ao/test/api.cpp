/*
Ao: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <iostream>
#include "catch.hpp"

#include "ao/tree/opcode.hpp"
#include "ao/tree/tree.hpp"
#include "ao/ao.h"

using namespace Kernel;

TEST_CASE("ao_opcode_enum")
{
    REQUIRE(ao_opcode_enum("min") == Opcode::MIN);
    REQUIRE(ao_opcode_enum("max") == Opcode::MAX);
    REQUIRE(ao_opcode_enum("VAR-X") == Opcode::VAR_X);

    REQUIRE(ao_opcode_enum("wat") == -1);
    REQUIRE(ao_opcode_enum("") == -1);
}

TEST_CASE("ao_tree")
{
    auto a = ao_tree_x();
    auto b = ao_tree_x();
    auto c = ao_tree_y();
    REQUIRE(a->id() == b->id());
    REQUIRE(b->id() != c->id());

    ao_tree_delete(a);
    ao_tree_delete(b);
    ao_tree_delete(c);
}

TEST_CASE("ao_tree_eval_f")
{
    auto a = ao_tree_x();
    auto b = ao_tree_y();
    auto c = ao_tree_binary(Opcode::DIV, a, b);

    REQUIRE(ao_tree_eval_f(c, {1,2,3}) == 0.5);
    REQUIRE(ao_tree_eval_f(c, {1,4,3}) == 0.25);
    REQUIRE(ao_tree_eval_f(c, {1,-1,3}) == -1);

    ao_tree_delete(a);
    ao_tree_delete(b);
    ao_tree_delete(c);
}

TEST_CASE("ao_tree_render_slice")
{
    auto x = ao_tree_x();
    auto y = ao_tree_y();
    auto x2 = ao_tree_unary(Opcode::SQUARE, x);
    auto y2 = ao_tree_unary(Opcode::SQUARE, y);
    auto r = ao_tree_binary(Opcode::ADD, x2, y2);
    auto one = ao_tree_const(1.0f);
    auto d = ao_tree_binary(Opcode::SUB, r, one);

    auto cs = ao_tree_render_slice(d, {{-2, 2}, {-2, 2}}, 0, 10);
    REQUIRE(cs->count == 1);
    REQUIRE(cs->cs[0].count > 0);
    float rmin = 2;
    float rmax = 0;
    for (unsigned i=0; i < cs->cs[0].count; ++i)
    {
        auto& v = cs->cs[0].pts[i];
        auto r = pow(v.x, 2) + pow(v.y, 2);
        rmin = fmin(rmin, r);
        rmax = fmax(rmax, r);
    }
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);

    for (auto t : {x, y, x2, y2, r, one, d})
    {
        ao_tree_delete(t);
    }
    ao_contours_delete(cs);
}

TEST_CASE("ao_tree_render_mesh")
{
    auto x = ao_tree_x();
    auto y = ao_tree_y();
    auto z = ao_tree_z();
    auto x2 = ao_tree_unary(Opcode::SQUARE, x);
    auto y2 = ao_tree_unary(Opcode::SQUARE, y);
    auto z2 = ao_tree_unary(Opcode::SQUARE, z);
    auto r_ = ao_tree_binary(Opcode::ADD, x2, y2);
    auto r = ao_tree_binary(Opcode::ADD, r_, z2);
    auto one = ao_tree_const(1.0f);
    auto d = ao_tree_binary(Opcode::SUB, r, one);

    auto m = ao_tree_render_mesh(d, {{-2, 2}, {-2, 2}, {-2, 2}}, 10);

    float rmin = 2;
    float rmax = 0;
    for (unsigned i=0; i < m->tri_count; ++i)
    {
        auto& t = m->tris[i];
        auto& va = m->verts[t.a];
        auto& vb = m->verts[t.b];
        auto& vc = m->verts[t.c];

        auto ra = pow(va.x, 2) + pow(va.y, 2) + pow(va.z, 2);
        auto rb = pow(vb.x, 2) + pow(vb.y, 2) + pow(vb.z, 2);
        auto rc = pow(vc.x, 2) + pow(vc.y, 2) + pow(vc.z, 2);

        rmin = fmin(fmin(fmin(rmin, ra), rb), rc);
        rmax = fmax(fmax(fmax(rmax, ra), rb), rc);
    }
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);

    for (auto t : {x, y, z, x2, y2, z2, r_, r, one, d})
    {
        ao_tree_delete(t);
    }
    ao_mesh_delete(m);
}

TEST_CASE("ao_tree_save/load")
{
    auto a = ao_tree_x();
    auto b = ao_tree_y();
    auto c = ao_tree_binary(Opcode::DIV, a, b);

    ao_tree_save(c, ".ao_tree.tmp");
    auto c_ = ao_tree_load(".ao_tree.tmp");
    REQUIRE(c_ != nullptr);
    REQUIRE(ao_tree_eq(c, c_));

    // Redirect stderr to avoid spurious print statements
    std::stringstream buffer;
    std::streambuf* old = std::cerr.rdbuf(buffer.rdbuf());
    auto f = ao_tree_load(".not_ao_tree.tmp");
    std::cerr.rdbuf(old);

    REQUIRE(f == nullptr);
}

TEST_CASE("ao_tree_render_pixels")
{
    auto x = ao_tree_x();
    auto y = ao_tree_y();
    auto z = ao_tree_z();
    auto x2 = ao_tree_unary(Opcode::SQUARE, x);
    auto y2 = ao_tree_unary(Opcode::SQUARE, y);
    auto z2 = ao_tree_unary(Opcode::SQUARE, z);
    auto r_ = ao_tree_binary(Opcode::ADD, x2, y2);
    auto r = ao_tree_binary(Opcode::ADD, r_, z2);
    auto one = ao_tree_const(1.0f);
    auto d = ao_tree_binary(Opcode::SUB, r, one);

    auto m = ao_tree_render_pixels(d, {{-2, 2}, {-2, 2}}, 0, 10);
    ao_pixels_delete(m);

    REQUIRE(true); // No crash!
}
