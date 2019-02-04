/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017-2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/dc/marching.hpp"

using namespace Kernel;

TEST_CASE("MarchingTable<2>")
{
    REQUIRE(MarchingTable<2>::mt.v.size() == 16);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < MarchingTable<2>::mt.v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = MarchingTable<2>::mt.v[i];
        REQUIRE(MarchingTable<2>::mt.v[i][0][0].first != -1);
    }

    REQUIRE(MarchingTable<2>::mt.v[0][0][0].first == -1);
    REQUIRE(MarchingTable<2>::mt.v[15][0][0].first == -1);
}

TEST_CASE("MarchingTable<3>")
{
    REQUIRE(MarchingTable<3>::mt.v.size() == 256);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < MarchingTable<3>::mt.v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = MarchingTable<3>::mt.v[i];
        REQUIRE(MarchingTable<3>::mt.v[i][0][0].first != -1);
    }

    REQUIRE(MarchingTable<3>::mt.v[0][0][0].first == -1);
    REQUIRE(MarchingTable<3>::mt.v[255][0][0].first == -1);
}
