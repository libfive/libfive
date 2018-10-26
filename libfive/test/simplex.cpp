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
#include "libfive/render/brep/simplex/simplex_mesher.hpp"
#include "libfive/render/brep/types.hpp"
#include "libfive/render/brep/dual.hpp"
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

    t->assignIndices();
}

TEST_CASE("SimplexTree<2>::assignIndices")
{
    auto c = circle(1);
    auto r = Region<2>({-1, -1}, {1, 1});

    auto t = SimplexTreePool<2>::build(c, r, 1.1, 1e-8, 1);
    REQUIRE(t.get() != nullptr);

    t->assignIndices();

    REQUIRE(t->isBranch());
    std::set<uint64_t> indices;
    for (auto& c : t->children) {
        REQUIRE(c.load() != nullptr);
        REQUIRE(!c.load()->isBranch());
        REQUIRE(c.load()->leaf != nullptr);
        for (auto& i : c.load()->leaf->index)
        {
            indices.insert(i);
        }
    }
    REQUIRE(indices.size() == 25);
    REQUIRE(*indices.begin() == 1);
    REQUIRE(*indices.rbegin() == 25);
}

TEST_CASE("SimplexTree<3>: types")
{
    auto c = sphere(0.5);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    auto t = SimplexTreePool<3>::build(c, r);
    REQUIRE(t.get() != nullptr);

    REQUIRE(t->isBranch());
    for (auto& c : t->children) {
        REQUIRE(c.load()->type == Interval::AMBIGUOUS);
    }
}

void test_corner_positions(const SimplexTree<3>* ptr, Region<3> r)
{
    std::list<std::pair<const SimplexTree<3>*, Region<3>>> todo;
    todo.push_back({ptr, r});

    unsigned checked_count = 0;
    while (todo.size())
    {
        auto task = todo.front();
        todo.pop_front();

        if (task.first->isBranch())
        {
            auto rs = task.second.subdivide();
            for (unsigned i=0; i < 8; ++i) {
                todo.push_back({task.first->children[i].load(), rs[i]});
            }
        }
        else if (task.first->leaf)
        {
            for (unsigned i=0; i < 8; ++i) {
                Eigen::Vector3d vt = task.first->leaf->vertices.row(
                        CornerIndex(i).neighbor().i);
                Eigen::Vector3d vr = task.second.corner(i);
                CAPTURE(vt);
                CAPTURE(vr);
                REQUIRE((vt - vr).norm() < 1e-6);
                checked_count++;
            }
        }
    }
    REQUIRE(checked_count > 0);
}

TEST_CASE("SimplexTree<3>: Corner positions")
{
    SECTION("Sphere")
    {
        auto c = sphere(0.5);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = SimplexTreePool<3>::build(c, r);
        REQUIRE(t.get() != nullptr);
        test_corner_positions(t.get(), r);
    }

    SECTION("Box (low-resolution)")
    {
        auto c = box({-0.4, -0.4, -0.4}, {0.4, 0.4, 0.4});
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = SimplexTreePool<3>::build(c, r, 0.4, 0, 1);
        test_corner_positions(t.get(), r);
    }
}

TEST_CASE("SimplexTree<3>::assignIndices")
{
    auto c = sphere(0.5);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    auto t = SimplexTreePool<3>::build(c, r, 1.1, 1e-8, 1);
    REQUIRE(t.get() != nullptr);

    t->assignIndices();

    REQUIRE(t->isBranch());
    std::set<uint64_t> indices;
    for (auto& c : t->children) {
        REQUIRE(c.load() != nullptr);
        REQUIRE(!c.load()->isBranch());
        REQUIRE(c.load()->leaf != nullptr);
        for (auto& i : c.load()->leaf->index)
        {
            indices.insert(i);
        }
    }
    REQUIRE(indices.size() == 125);
    REQUIRE(*indices.begin() == 1);
    REQUIRE(*indices.rbegin() == 125);
}

TEST_CASE("SimplexTree<3>::leafLevel")
{
    auto c = sphere(0.5);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    auto t = SimplexTreePool<3>::build(c, r, 1.1, 1e-8, 1);
    REQUIRE(t->isBranch());
    for (auto& c : t->children) {
        REQUIRE(c.load()->leafLevel() == 0);
    }
}

TEST_CASE("SimplexMesher (smoke test)")
{
    auto c = sphere(0.5);
    auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

    auto t = SimplexTreePool<3>::build(c, r, 1.1, 1e-8, 1);
    REQUIRE(t->isBranch());
    for (auto& c : t->children) {
        REQUIRE(c.load()->type == Interval::AMBIGUOUS);
    }
    t->assignIndices();

    Mesh m;
    XTreeEvaluator eval(c);
    auto mesher = SimplexMesher(m, &eval);
    Dual<3>::walk(t.get(), mesher);

    REQUIRE(m.branes.size() > 0);
    REQUIRE(m.verts.size() > 1);
    m.saveSTL("out.stl");
}

void test_edge_pairs(const Mesh& m) {
    // Every edge must be shared by two triangles
    // We build a bitfield here, counting forward and reverse edges
    std::map<std::pair<int, int>, int> edges;
    for (const auto& t : m.branes) {
        for (unsigned i=0; i < 3; ++i) {
            const auto a = t[i];
            const auto b = t[(i + 1) % 3];
            auto key = std::make_pair(std::min(a, b), std::max(a, b));
            if (!edges.count(key)) {
                edges.insert({key, 0});
            }
            if (a < b)
            {
                REQUIRE((edges[key] & 1) == 0);
                edges[key] |= 1;
            }
            else
            {
                REQUIRE((edges[key] & 2) == 0);
                edges[key] |= 2;
            }
        }
    }
    for (auto& p : edges) {
        CAPTURE(p.first.first);
        CAPTURE(p.first.second);
        CAPTURE(m.verts[p.first.first]);
        CAPTURE(m.verts[p.first.second]);
        REQUIRE(p.second == 3);
    }
}

TEST_CASE("SimplexMesher: edge pairing")
{
    /*
    SECTION("Sphere (low resolution)")
    {
        auto c = sphere(0.4);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = SimplexTreePool<3>::build(c, r, 1.1, 0, 1);
        REQUIRE(t->isBranch());
        for (auto& c : t->children) {
            REQUIRE(c.load()->type == Interval::AMBIGUOUS);
        }
        t->assignIndices();

        Mesh m;
        XTreeEvaluator eval(c);
        auto mesher = SimplexMesher(m, &eval);
        Dual<3>::walk(t.get(), mesher);

        test_edge_pairs(m);
    }

    SECTION("Sphere (higher-resolution)")
    {
        auto c = sphere(0.4);
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = SimplexTreePool<3>::build(c, r, 0.4, 0, 1);
        t->assignIndices();

        Mesh m;
        XTreeEvaluator eval(c);
        auto mesher = SimplexMesher(m, &eval);
        Dual<3>::walk(t.get(), mesher);

        test_edge_pairs(m);
    }
    */

    SECTION("Box (low-resolution)")
    {
        auto c = box({-0.4, -0.4, -0.4}, {0.4, 0.4, 0.4});
        auto r = Region<3>({-1, -1, -1}, {1, 1, 1});

        auto t = SimplexTreePool<3>::build(c, r, 0.4, 0, 1);
        t->assignIndices();

        Mesh m;
        XTreeEvaluator eval(c);
        auto mesher = SimplexMesher(m, &eval);
        Dual<3>::walk(t.get(), mesher);

        m.saveSTL("out.stl");

        test_edge_pairs(m);
    }
}
