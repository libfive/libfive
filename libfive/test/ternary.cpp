/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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
#include "catch.hpp"

#include "libfive/render/simplex/ternary.hpp"

using namespace Kernel;

TEST_CASE("ternary<3>")
{
    std::array<int, 3> i;

    i = {{-1, -1, -1}};
    REQUIRE(ternary<3>(0) == i);

    i = {{1, -1, -1}};
    REQUIRE(ternary<3>(1) == i);

    i = {{0, -1, -1}};
    REQUIRE(ternary<3>(2) == i);

    i = {{0, 0, 0}};
    REQUIRE(ternary<3>(26) == i);
}

TEST_CASE("ternary<2>")
{
    std::array<int, 2> i;

    i = {{-1, -1}};
    REQUIRE(ternary<2>(0) == i);

    i = {{1, -1}};
    REQUIRE(ternary<2>(1) == i);

    i = {{0, -1}};
    REQUIRE(ternary<2>(2) == i);

    i = {{0, 0}};
    REQUIRE(ternary<2>(8) == i);
}
