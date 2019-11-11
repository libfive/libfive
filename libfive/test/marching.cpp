/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017-2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/dc/marching.hpp"

using namespace libfive;

TEST_CASE("MarchingTable<2>")
{
    // Make sure that we have built every item in the table
    for (unsigned i=1; i < ipow(2, _verts(2)) - 1; ++i)
    {
        CAPTURE(i);
        REQUIRE(MarchingTable<2>::v(i)[0][0].first != -1);
    }

    REQUIRE(MarchingTable<2>::v(0)[0][0].first == -1);
    REQUIRE(MarchingTable<2>::v(15)[0][0].first == -1);
}

TEST_CASE("MarchingTable<3>")
{
    // Make sure that we have built every item in the table
    for (unsigned i=1; i < ipow(2, _verts(3)) - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = MarchingTable<3>::v(i);
        (void)_t;
        REQUIRE(MarchingTable<3>::v(i)[0][0].first != -1);
    }

    REQUIRE(MarchingTable<3>::v(0)[0][0].first == -1);
    REQUIRE(MarchingTable<3>::v(255)[0][0].first == -1);
}
