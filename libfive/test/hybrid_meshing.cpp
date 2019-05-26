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
#include "libfive/render/brep/hybrid/hybrid_debug.hpp"
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

#if 0 // Uncomment to save debug meshes
    m->saveSTL("out.stl");
    auto g = Dual<3>::walk<HybridDebugMesher>(t, settings, c);
    g->saveSTL("grid.stl");
#endif

    CHECK_EDGE_PAIRS(*m);
}

TEST_CASE("HybridTree<3>: edge vertex placement")
{
    // In this test, we build a single cell where one of the edges
    // has a sharp feature that's off-center, then confirm that
    // that edge's subspace vertex ends up correctly positioned.
    auto c = sphere(0.05, {-1, -1, 0.8});
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    BRepSettings settings;
    settings.min_feature = 5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);

    CAPTURE(t->leaf->pos.col(18).transpose());
    REQUIRE((t->leaf->pos.col(18) - Eigen::Vector3d(-1, -1, 0.8)).norm() < 0.01);

    REQUIRE(t->leaf->intersection(18));
    for (unsigned i=0; i < ipow(3, 3); ++i) {
        if (i != 18 && NeighborIndex(i).dimension() == 1) {
            CAPTURE(i);
            REQUIRE(!t->leaf->intersection(i));
        }
    }
}

TEST_CASE("HybridTree<2>: subspace vertex placement")
{
    SECTION("Offset circle") {
        // In this test, we build a single cell where one of the edges
        // has a sharp feature that's off-center, then confirm that
        // that edge's subspace vertex ends up correctly positioned.
        auto c = circle(0.05, {-0.9, 0.7});
        auto r = Region<2>({-1, -1}, {1, 1});

        BRepSettings settings;
        settings.min_feature = 5;
        settings.workers = 1;
        auto t = HybridTreePool<2>::build(c, r, settings);

        CAPTURE(t->leaf->pos.col(8));
        REQUIRE((t->leaf->pos.col(8) - Eigen::Vector2d(-0.9, 0.7)).norm() < 0.01);

        REQUIRE(!t->leaf->intersection(0));
        REQUIRE(!t->leaf->intersection(1));
        REQUIRE(!t->leaf->intersection(2));
        REQUIRE(!t->leaf->intersection(3));
        REQUIRE(!t->leaf->intersection(4));
        REQUIRE(!t->leaf->intersection(5));
        // The edge 6 with partial circle touching may or may not be an intersection
        REQUIRE(!t->leaf->intersection(7));
        REQUIRE( t->leaf->intersection(8));
    }

    SECTION("Rectangle") {
        auto c = box({-0.6, -0.8, -1.0}, {0.7, 0.9, 1.0});
        auto r = Region<2>({-1, -1}, {0, 0});

        BRepSettings settings;
        settings.min_feature = 5;
        settings.workers = 1;
        auto t = HybridTreePool<2>::build(c, r, settings);

        CAPTURE(t->leaf->pos.col(8));
        REQUIRE((t->leaf->pos.col(8) - Eigen::Vector2d(-0.6, -0.8)).norm() < 0.01);
    }
}

TEST_CASE("HybridMesher<3>: cylinder meshing")
{
    auto c = cylinder(1, 1);
    auto r = Region<3>({-3, -3, -3}, {3, 3, 3});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);

#if 1 // Uncomment to save debug meshes
    m->saveSTL("out.stl");
    auto g = Dual<3>::walk<HybridDebugMesher>(t, settings, c);
    g->saveSTL("grid.stl");
#endif
}
