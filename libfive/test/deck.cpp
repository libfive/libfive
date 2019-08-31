/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/deck.hpp"

using namespace libfive;

TEST_CASE("Deck::num_clauses")
{
    Deck t(Tree::X() + 1);
    REQUIRE(t.num_clauses == 5); // X, Y, Z, 1, +
}

TEST_CASE("Deck::XYZ")
{
    Deck t(Tree::X() + 1);
    REQUIRE(t.X == 3);
    REQUIRE(t.Y == 4);
    REQUIRE(t.Z == 5);
}

TEST_CASE("Deck::constants")
{
    Deck t(Tree::X() + 5);
    REQUIRE(t.constants.size() == 1);

    CAPTURE(t.constants.begin()->first);
    CAPTURE(t.constants.begin()->second);
    REQUIRE(t.constants.at(2) == 5.0f);
}

#if LIBFIVE_USE_NARY_OPS
TEST_CASE("Deck: NARY_MIN collapsing")
{
    Deck t(min(Tree::X(), min(Tree::Y(), Tree::Z())));
    REQUIRE(t.num_clauses == 4); // X, Y, Z, n-ary min

    // min(x, min(y, z)) + 2 + min(y, z)
}
#endif
