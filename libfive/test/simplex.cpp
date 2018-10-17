/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/simplex/simplex_pool.hpp"
#include "libfive/render/brep/simplex/solver.hpp"
#include "util/shapes.hpp"

using namespace Kernel;
using namespace Kernel::SimplexSolver;

TEST_CASE("simplexDimension")
{
    REQUIRE(simplexDimension(0) == 0);
    REQUIRE(simplexDimension(1) == 0);
    REQUIRE(simplexDimension(2) == 1);
    REQUIRE(simplexDimension(5) == 1);
    REQUIRE(simplexDimension(6) == 1);
    REQUIRE(simplexDimension(8) == 2);
    REQUIRE(simplexDimension(9) == 0);
    REQUIRE(simplexDimension(11) == 1);
    REQUIRE(simplexDimension(17) == 2);
    REQUIRE(simplexDimension(18) == 1);
    REQUIRE(simplexDimension(20) == 2);
    REQUIRE(simplexDimension(26) == 3);
}

TEST_CASE("cornerToSimplex")
{
    REQUIRE(cornerToSimplex(0) == 0);
    REQUIRE(cornerToSimplex(1) == 1);
    REQUIRE(cornerToSimplex(2) == 3);
    REQUIRE(cornerToSimplex(3) == 4);
    REQUIRE(cornerToSimplex(4) == 9);
    REQUIRE(cornerToSimplex(5) == 10);
    REQUIRE(cornerToSimplex(6) == 12);
    REQUIRE(cornerToSimplex(7) == 13);
}

TEST_CASE("SimplexPool")
{
    auto c = circle(1);
    auto r = Region<2>({-1, -1}, {1, 1});

    auto t = SimplexTreePool<2>::build(c, r, 0.1, 1e-8, 1);
    REQUIRE(t.get() != nullptr);
}
