/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/manifold_tables.hpp"

using namespace Kernel;

TEST_CASE("ManifoldTables<2>::manifold")
{
    /*  Remember, corners are numbered
     *
     *      3--4--5
     *      |     |
     *      6     7
     *      |     |
     *      0--2--1
     */

    // We're going to just do a bunch of spot-checks here
    REQUIRE(!ManifoldTables<2>::manifold(0));
    REQUIRE(ManifoldTables<2>::manifold(1 << 0));
    REQUIRE(ManifoldTables<2>::manifold(
        (1 << 0) | (1 << 6) | (1 << 3)));
    REQUIRE(!ManifoldTables<2>::manifold(
        (1 << 0) | (1 << 6) | (1 << 3) | (1 << 7)));
    REQUIRE(!ManifoldTables<2>::manifold(
        (1 << 0) | (1 << 7)));
    REQUIRE(ManifoldTables<2>::manifold(
        (1 << 0) | (1 << 2) | (1 << 1) | (1 << 7)));
    REQUIRE(ManifoldTables<2>::manifold(
        (1 << 3) | (1 << 4) | (1 << 5)));
    REQUIRE(!ManifoldTables<2>::manifold(
        (1 << 6) | (1 << 4)));
    REQUIRE(!ManifoldTables<2>::manifold(
        (1 << 0) | (1 << 1) | (1 << 2) |
        (1 << 3) | (1 << 4) | (1 << 5) |
        (1 << 6) | (1 << 7)));
}

TEST_CASE("ManifoldTables<3>::manifold")
{
    // All of the 2D case should still be true
    REQUIRE(!ManifoldTables<3>::manifold(0));
    REQUIRE(ManifoldTables<3>::manifold(1 << 0));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 6) | (1 << 3)));
    REQUIRE(!ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 6) | (1 << 3) | (1 << 7)));
    REQUIRE(!ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 7)));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 2) | (1 << 1) | (1 << 7)));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 3) | (1 << 4) | (1 << 5)));
    REQUIRE(!ManifoldTables<3>::manifold(
        (1 << 6) | (1 << 4)));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 8)));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 8) | (1 << 5)));
    REQUIRE(ManifoldTables<3>::manifold(
        (1 << 0) | (1 << 1) | (1 << 2) |
        (1 << 3) | (1 << 4) | (1 << 5) |
        (1 << 6) | (1 << 7)));
}
