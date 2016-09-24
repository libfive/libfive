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

TEST_CASE("Joining two trees")
{
    auto t = Tree(Opcode::ADD, Tree::X(), Tree(1));

    REQUIRE(t.opcode() == Opcode::ADD);
    REQUIRE(t.lhs().opcode() == Opcode::VAR_X);
    REQUIRE(t.rhs().opcode() == Opcode::CONST);
    REQUIRE(t.rhs().value() == 1);
}
