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
#include "libfive/render/brep/types.hpp"
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

TEST_CASE("NeighborIndex::dimension")
{
    REQUIRE(NeighborIndex(0).dimension() == 0);
    REQUIRE(NeighborIndex(1).dimension() == 0);
    REQUIRE(NeighborIndex(2).dimension() == 1);
    REQUIRE(NeighborIndex(5).dimension() == 1);
    REQUIRE(NeighborIndex(6).dimension() == 1);
    REQUIRE(NeighborIndex(8).dimension() == 2);
    REQUIRE(NeighborIndex(9).dimension() == 0);
    REQUIRE(NeighborIndex(11).dimension() == 1);
    REQUIRE(NeighborIndex(17).dimension() == 2);
    REQUIRE(NeighborIndex(18).dimension() == 1);
    REQUIRE(NeighborIndex(20).dimension() == 2);
    REQUIRE(NeighborIndex(26).dimension() == 3);
}

TEST_CASE("NeighborIndex::contains")
{
    REQUIRE(NeighborIndex(0).contains(CornerIndex(0)));

    REQUIRE(NeighborIndex(1).contains(CornerIndex(1)));
    REQUIRE(!NeighborIndex(1).contains(CornerIndex(0)));
    REQUIRE(!NeighborIndex(1).contains(CornerIndex(2)));
    REQUIRE(!NeighborIndex(1).contains(CornerIndex(3)));

    REQUIRE(NeighborIndex(2).contains(CornerIndex(0)));
    REQUIRE(NeighborIndex(2).contains(CornerIndex(1)));
    REQUIRE(!NeighborIndex(2).contains(CornerIndex(2)));
    REQUIRE(!NeighborIndex(2).contains(CornerIndex(3)));

    REQUIRE(!NeighborIndex(5).contains(CornerIndex(0)));
    REQUIRE(!NeighborIndex(5).contains(CornerIndex(1)));
    REQUIRE(NeighborIndex(5).contains(CornerIndex(2)));
    REQUIRE(NeighborIndex(5).contains(CornerIndex(3)));

    for (unsigned i=0; i < 8; ++i) {
        REQUIRE(NeighborIndex(26).contains(CornerIndex(i)));
    }
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

TEST_CASE("CornerIndex::neighbor")
{
    REQUIRE(CornerIndex(0).neighbor().i == 0);
    REQUIRE(CornerIndex(1).neighbor().i == 1);
    REQUIRE(CornerIndex(2).neighbor().i == 3);
    REQUIRE(CornerIndex(3).neighbor().i == 4);
    REQUIRE(CornerIndex(4).neighbor().i == 9);
    REQUIRE(CornerIndex(5).neighbor().i == 10);
    REQUIRE(CornerIndex(6).neighbor().i == 12);
    REQUIRE(CornerIndex(7).neighbor().i == 13);
}

TEST_CASE("simplexUnion")
{
    REQUIRE(simplexUnion(0, 1) == 2);
    REQUIRE(simplexUnion(0, 2) == 2);
    REQUIRE(simplexUnion(0, 4) == 8);
    REQUIRE(simplexUnion(2, 4) == 8);
    REQUIRE(simplexUnion(1, 4) == 7);
    REQUIRE(simplexUnion(9, 0) == 18);
}

TEST_CASE("NeighborIndex::operator|")
{
    REQUIRE((NeighborIndex(0) | NeighborIndex(1)).i == 2);
    REQUIRE((NeighborIndex(0)| NeighborIndex(2)).i == 2);
    REQUIRE((NeighborIndex(0)| NeighborIndex(4)).i == 8);
    REQUIRE((NeighborIndex(2)| NeighborIndex(4)).i == 8);
    REQUIRE((NeighborIndex(1)| NeighborIndex(4)).i == 7);
    REQUIRE((NeighborIndex(9)| NeighborIndex(0)).i == 18);
}


TEST_CASE("NeighborIndex::fromPosAndFloating")
{
    for (int i=0; i < pow(3, 3); ++i)
    {
        NeighborIndex n(i);
        CAPTURE(n.pos())
        CAPTURE(n.floating())
        auto m = NeighborIndex::fromPosAndFloating(n.pos(), n.floating());
        REQUIRE(i == m.i);
    }
}

TEST_CASE("SimplexPool")
{
    auto c = circle(1);
    auto r = Region<2>({-1, -1}, {1, 1});

    auto t = SimplexTreePool<2>::build(c, r, 0.1, 1e-8, 1);
    REQUIRE(t.get() != nullptr);
}
