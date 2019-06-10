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

#include <set>

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
    auto c = sphere(0.05, {-1, -1, 0.8f});
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    BRepSettings settings;
    settings.min_feature = 5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);

    CAPTURE(t->leaf->vertex_pos.col(18).transpose());
    REQUIRE((t->leaf->vertex_pos.col(18) - Eigen::Vector3d(-1, -1, 0.8)).norm() < 0.01);

    REQUIRE(t->leaf->has_surface_qef[18]);
    for (unsigned i=0; i < ipow(3, 3); ++i) {
        if (i != 18 && NeighborIndex(i).dimension() == 1) {
            CAPTURE(i);
            REQUIRE(!t->leaf->has_surface_qef[i]);
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

        CAPTURE(t->leaf->vertex_pos.col(8));
        REQUIRE((t->leaf->vertex_pos.col(8) - Eigen::Vector2d(-0.9, 0.7)).norm() < 0.01);

        REQUIRE(!t->leaf->has_surface_qef[0]);
        REQUIRE(!t->leaf->has_surface_qef[1]);
        REQUIRE(!t->leaf->has_surface_qef[2]);
        REQUIRE(!t->leaf->has_surface_qef[3]);
        REQUIRE(!t->leaf->has_surface_qef[4]);
        REQUIRE(!t->leaf->has_surface_qef[5]);
        // The edge 6 with partial circle touching may or may not be an intersection
        REQUIRE(!t->leaf->has_surface_qef[7]);
        REQUIRE( t->leaf->has_surface_qef[8]);
    }

    SECTION("Rectangle") {
        auto c = box({-0.6f, -0.8f, -1.0}, {0.7f, 0.9f, 1.0});
        auto r = Region<2>({-1, -1}, {0, 0});

        BRepSettings settings;
        settings.min_feature = 5;
        settings.workers = 1;
        auto t = HybridTreePool<2>::build(c, r, settings);

        CAPTURE(t->leaf->vertex_pos.col(8));
        REQUIRE((t->leaf->vertex_pos.col(8) - Eigen::Vector2d(-0.6, -0.8)).norm() < 0.01);
    }
}

template <unsigned N>
void print_debug_leaf(const HybridTree<N>* t) {
    if (t->leaf) {
        for (unsigned i=0; i < ipow(3, N); ++i) {
            std::cout << i << ":\n";
            std::cout << " inside: " << t->leaf->inside[i] << "\n";
            std::cout << " surface: " << t->leaf->vertex_on_surface[i] << "\n";
            std::cout << " pos: " << t->leaf->vertex_pos.col(i).transpose() << "\n";
        }
    }
}

void save_debug_mesh(const Tree c, const Root<HybridTree<3>>& t,
                     const BRepSettings& settings, const Mesh* m)
{
    m->saveSTL("out.stl");
    auto g = Dual<3>::walk<HybridDebugMesher>(t, settings, c);
    g->saveSTL("grid.stl");
}

TEST_CASE("HybridMesher<3>: cylinder meshing")
{
    auto c = cylinder(1.3, 1.3);
    auto r = Region<3>({-3, -3, -3}, {3, 3, 3});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);

#if 0 // Uncomment to save debug meshes
    print_debug_leaf(t.get());
    save_debug_mesh(c, t, settings, m.get());
#endif
}

TEST_CASE("HybridMesher<3>: cube meshing")
{
    auto c = rotate2d(box({-1.4f, -1.3f, -1.5}, {1.2f, 1.4f, 1.2f}), 0.7);
    auto r = Region<3>({-2.25, 0, 1.125}, {-1.875, 0.375, 1.5});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);

#if 0 // Uncomment to save debug meshes
    print_debug_leaf(t.get());
    save_debug_mesh(c, t, settings, m.get());
#endif

    // Require that the cube's corner is on its actual corner
    REQUIRE(t->leaf != nullptr);
    const Eigen::Vector3d v = t->leaf->vertex_pos.col(26);
    CAPTURE(v.transpose());
    REQUIRE(v.x() == Approx( cos(0.7) * -1.4 - sin(0.7) * 1.4));
    REQUIRE(v.y() == Approx( sin(0.7) * -1.4 + cos(0.7) * 1.4));
    REQUIRE(v.z() == Approx(1.2));
}

TEST_CASE("HybridMesher<3>: cylinder cutout meshing")
{
    auto c = max(box({-1, -1, -1}, {1.4f, 1.1f, 1}),
            -cylinder(1.2, 1.3, {0, 0, 0.01f}));
    auto r = Region<3>({0, 0, 0}, {3, 3, 3});
    //auto r = Region<3>({1.125, 0.0, 0.9385}, {1.3125, 0.1875, 1.125});
    //auto r = Region<3>({0.46875, 1.03125, 0}, {0.5625, 1.125, 0.09375});

    BRepSettings settings;
    settings.min_feature = 0.125;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);

#if 0 // Uncomment to save debug meshes
    print_debug_leaf(t.get());
    save_debug_mesh(c, t, settings, m.get());
#endif
}

TEST_CASE("HybridMesher<3>: rotated cylinder meshing")
{
    auto c = rotate_x(-cylinder(1, 1, {0, 0, -0.5}), 0.4);
    //auto r = Region<3>({-3, -3, -3}, {3, 3, 3});
    auto r = Region<3>({-1.125, 0.375, 0.375}, {-0.75, 0.75, 0.75});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = HybridTreePool<3>::build(c, r, settings);
    t->assignIndices(settings);

    settings.workers = 8;
    auto m = Dual<3>::walk<HybridMesher>(t, settings, c);

#if 0 // Uncomment to save debug meshes
    print_debug_leaf(t.get());
    save_debug_mesh(c, t, settings, m.get());
#endif
}
