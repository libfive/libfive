/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/vol/vol_worker_pool.hpp"

using namespace libfive;

TEST_CASE("VolTree: basic behavior")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = VolWorkerPool::build(a, Region<3>({1, 1, 1}, {2, 2, 2}), BRepSettings());
        REQUIRE(e->type == Interval::EMPTY);
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = VolWorkerPool::build(a, Region<3>({-3, -3, -3}, {-1, -1, -1}), BRepSettings());
        REQUIRE(e->type == Interval::FILLED);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = VolWorkerPool::build(a, Region<3>({-3, -3, -3}, {1, 1, 1}), BRepSettings());
        REQUIRE(ta->type == Interval::AMBIGUOUS);
    }

    SECTION("Empty (single leaf)")
    {
        BRepSettings b;
        b.min_feature = 100;
        Tree a = min(Tree::X(), Tree::Y());
        auto e = VolWorkerPool::build(a, Region<3>({1, 1, 1}, {2, 2, 2}), b);
        REQUIRE(e->type == Interval::EMPTY);
    }
}

