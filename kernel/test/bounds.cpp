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

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/token.hpp"
#include "ao/kernel/eval/interval.hpp"

TEST_CASE("Bounds")
{
    Store s;

    Token* t = s.bounded(s.X(), {{-1, -2, -3}, {3, 4, 5}});

    bool success = false;
    auto b = t->getBounds(&success);
    REQUIRE(success);

    REQUIRE(b.lower.x == -1);
    REQUIRE(b.lower.y == -2);
    REQUIRE(b.lower.z == -3);

    REQUIRE(b.upper.x ==  3);
    REQUIRE(b.upper.y ==  4);
    REQUIRE(b.upper.z ==  5);
}
