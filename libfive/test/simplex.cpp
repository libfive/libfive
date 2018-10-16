/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/simplex/simplex_pool.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SimplexPool")
{
    auto c = circle(1);
    auto r = Region<2>({-1, -1}, {1, 1});

    auto t = SimplexTreePool<2>::build(c, r);
    REQUIRE(t.get() != nullptr);
}
