/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/simplex/surface_edge_map.hpp"

using namespace Kernel;

TEST_CASE("SurfaceEdgeMap::insert") {
    SurfaceEdgeMap<16> s;

    s.insert({1, 2}, 4);
    REQUIRE(s.size() == 1);

    s.insert({1, 2}, 4);
    REQUIRE(s.size() == 1);

    s.insert({1, 3}, 4);
    REQUIRE(s.size() == 2);
}

TEST_CASE("SurfaceEdgeMap::find") {
    SurfaceEdgeMap<16> s;

    s.insert({1, 3}, 4);
    REQUIRE(s.find({1, 3}) == 4);
}
