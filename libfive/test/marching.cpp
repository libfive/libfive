/*
libfive: a CAD kernel for modeling with implicit functions
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
#include "catch.hpp"

#include "libfive/render/brep/marching.hpp"

using namespace Kernel;

TEST_CASE("Marching::buildTable<2>")
{
    auto t = Marching::buildTable<2>();
    REQUIRE(t->v.size() == 16);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = t->v[i];
        REQUIRE(t->v[i][0][0].first != -1);
    }

    REQUIRE(t->v[0][0][0].first == -1);
    REQUIRE(t->v[15][0][0].first == -1);
}

TEST_CASE("Marching::buildTable<3>")
{
    auto t = Marching::buildTable<3>();
    REQUIRE(t->v.size() == 256);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = t->v[i];
        REQUIRE(t->v[i][0][0].first != -1);
    }

    REQUIRE(t->v[0][0][0].first == -1);
    REQUIRE(t->v[255][0][0].first == -1);
}
