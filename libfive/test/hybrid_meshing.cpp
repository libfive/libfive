/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/hybrid/hybrid_pool.hpp"
#include "libfive/render/brep/hybrid/hybrid_mesher.hpp"
#include "libfive/render/brep/dual.hpp"

#include "util/shapes.hpp"
#include "util/mesh_checks.hpp"

using namespace Kernel;

TEST_CASE("HybridPool::build (smoke test)")
{
    BRepSettings settings;
    settings.min_feature = 5;
    settings.workers = 1;

    SECTION("2D")
    {
        auto c = circle(1);
        auto r = Region<2>({-1, -1}, {1, 1});

        auto t = HybridTreePool<2>::build(c, r, settings);
        REQUIRE(t.get() != nullptr);
    }

    SECTION("3D")
    {
        auto c = sphere(1);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = HybridTreePool<3>::build(c, r, settings);
        REQUIRE(t.get() != nullptr);
    }
}

TEST_CASE("HybridTree::assignIndices")
{
    BRepSettings settings;
    settings.min_feature = 1;
    settings.workers = 1;

    SECTION("2D") {
        auto c = circle(1);
        auto r = Region<2>({-1, -1}, {1, 1});

        auto t = HybridTreePool<2>::build(c, r, settings);
        REQUIRE(t->isBranch());
        for (auto& c : t->children) {
            REQUIRE(!c.load()->isBranch());
            REQUIRE(c.load()->type == Interval::AMBIGUOUS);
        }
        t->assignIndices(settings);

        std::set<uint64_t> indices;
        auto f = [&](const HybridTree<2>* t) {
            if (t->leaf) {
                for (auto& i: t->leaf->index) {
                    indices.insert(i);
                }
            }
        };
        t->map(f);
        REQUIRE(indices.size() == 25);
        REQUIRE(*indices.begin() == 1);
        REQUIRE(*indices.rbegin() == 25);
    }

    SECTION("3D") {
        auto c = sphere(1);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = HybridTreePool<3>::build(c, r, settings);
        REQUIRE(t->isBranch());
        for (auto& c : t->children) {
            REQUIRE(!c.load()->isBranch());
            REQUIRE(c.load()->type == Interval::AMBIGUOUS);
        }
        t->assignIndices(settings);

        std::set<uint64_t> indices;
        auto f = [&](const HybridTree<3>* t) {
            if (t->leaf) {
                for (auto& i: t->leaf->index) {
                    indices.insert(i);
                }
            }
        };
        t->map(f);
        REQUIRE(indices.size() == 125);
        REQUIRE(*indices.begin() == 1);
        REQUIRE(*indices.rbegin() == 125);
    }
}

TEST_CASE("HybridMesher<3>: smoke test")
{
    auto c = sphere(1);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    SECTION("Single cell")
    {
        BRepSettings settings;
        settings.min_feature = 5;
        settings.workers = 1;

        auto t = HybridTreePool<3>::build(c, r, settings);
        t->assignIndices(settings);

        settings.workers = 8;
        auto m = Dual<3>::walk<HybridMesher>(t, settings, c);
        REQUIRE(m->branes.size() > 0);
        REQUIRE(m->verts.size() > 0);
    }

    SECTION("Recursive")
    {
        BRepSettings settings;
        settings.min_feature = 0.1;
        settings.workers = 1;

        auto t = HybridTreePool<3>::build(c, r, settings);
        t->assignIndices(settings);

        settings.workers = 8;
        auto m = Dual<3>::walk<HybridMesher>(t, settings, c);
        REQUIRE(m->branes.size() > 0);
        REQUIRE(m->verts.size() > 0);
    }
}

TEST_CASE("HybridMesher<3>: mesh manifoldness")
{
    auto c = sphere(0.8);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);
    m->saveSTL("out.stl");
    CHECK_EDGE_PAIRS(*m);
}
