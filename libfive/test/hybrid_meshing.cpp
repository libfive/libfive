/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/hybrid/hybrid_pool.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("HybridPool::build (smoke test)")
{
    SECTION("2D") {
        auto c = circle(1);
        auto r = Region<2>({-1, -1}, {1, 1});

        auto t = HybridTreePool<2>::build(c, r, 5, 1e-8, 1);
        REQUIRE(t.get() != nullptr);
    }

    SECTION("3D") {
        auto c = circle(1);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = HybridTreePool<3>::build(c, r, 5, 1e-8, 1);
        REQUIRE(t.get() != nullptr);
    }
}

TEST_CASE("HybridTree::assignIndices")
{
    SECTION("2D") {
        auto c = circle(1);
        auto r = Region<2>({-1, -1}, {1, 1});

        auto t = HybridTreePool<2>::build(c, r, 1, 1e-8, 1);
        REQUIRE(t->isBranch());
        for (auto& c : t->children) {
            REQUIRE(!c.load()->isBranch());
            REQUIRE(c.load()->type == Interval::AMBIGUOUS);
        }
        t->assignIndices();

        uint32_t max_index = 0;
        auto f = [&](const HybridTree<2>* t) {
            if (t->leaf) {
                for (auto& i: t->leaf->index) {
                    REQUIRE(i != 0);
                    max_index = std::max(max_index, i);
                }
            }
        };
        t->map(f);
        REQUIRE(max_index == 25);
    }

    SECTION("3D") {
        auto c = sphere(1);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = HybridTreePool<3>::build(c, r, 1, 1e-8, 1);
        REQUIRE(t->isBranch());
        for (auto& c : t->children) {
            REQUIRE(!c.load()->isBranch());
            REQUIRE(c.load()->type == Interval::AMBIGUOUS);
        }
        t->assignIndices();

        uint32_t max_index = 0;
        auto f = [&](const HybridTree<3>* t) {
            if (t->leaf) {
                for (auto& i: t->leaf->index) {
                    REQUIRE(i != 0);
                    max_index = std::max(max_index, i);
                }
            }
        };
        t->map(f);
        REQUIRE(max_index == 125);
    }
}
