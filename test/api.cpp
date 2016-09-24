/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <catch/catch.hpp>

#include "ao/kernel/tree/tree.hpp"

#include "api.hpp"

TEST_CASE("struct contours")
{
    Tree t = Tree::operation(Opcode::SUB,
             Tree::operation(Opcode::ADD, Tree::operation(Opcode::MUL, Tree::X(), Tree::X()),
                                          Tree::operation(Opcode::MUL, Tree::Y(), Tree::Y())),
                 Tree::constant(0.5));

    struct contours* cs = tree_render_slice(&t, -1, 1, -1, 1, 0, 50);
    REQUIRE(cs->size == 1);

    float min = 1;
    float max = 0;
    for (uint32_t i=0; i < cs->sizes[0]; ++i)
    {
        auto r = pow(cs->xs[0][i], 2) + pow(cs->ys[0][i], 2);
        min = fmin(min, r);
        max = fmax(max, r);
    }
    REQUIRE(max < 0.51);
    REQUIRE(min > 0.49);
    contours_delete(cs);
}
