/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/vol/vol_worker_pool.hpp"
#include "libfive/render/brep/contours.hpp"
#include "util/shapes.hpp"

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

TEST_CASE("VolTree: use as an accelerator")
{
    auto s = sphere(0.8);
    Region<3> r({-1, -1, -1}, {1, 1, 1});

    BRepSettings b;
    auto e = VolWorkerPool::build(s, r, b);
    REQUIRE(e->isBranch());

    for (float p : {0.9, 0.8, 0.7, 0.6}) {
        Region<2> q({-1, -1}, {1, 1}, Region<2>::Perp(p));
        BRepSettings c;
        c.vol = e.get();
        c.workers = 1;

        auto m = Contours::render(s, q, c);
    }
    REQUIRE(true);
}
