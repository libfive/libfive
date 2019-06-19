/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <future>

#include "catch.hpp"

#include "libfive/eval/eval_xtree.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/dc/dc_pool.hpp"
#include "libfive/render/axes.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("DCTree<2>::vert")
{
    SECTION("Vertex positioning (with two planes)")
    {
        Tree a = min(Tree::X() + 0.1, Tree::Y() - 0.2);
        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {1, 1}), BRepSettings());
        REQUIRE(ta->vert().x() == Approx(-0.1));
        REQUIRE(ta->vert().y() == Approx(0.2));
    }
}

TEST_CASE("DCTree<2>::type")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = DCPool<2>::build(a, Region<2>({1, 1}, {2, 2}), BRepSettings());
        REQUIRE(e->type == Interval::EMPTY);
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = DCPool<2>::build(a, Region<2>({-3, -3}, {-1, -1}), BRepSettings());
        REQUIRE(e->type == Interval::FILLED);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {1, 1}), BRepSettings());
        REQUIRE(ta->type == Interval::AMBIGUOUS);
    }
}

TEST_CASE("DCTree<2>::isBranch")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = DCPool<2>::build(a, Region<2>({1, 1}, {2, 2}), BRepSettings());
        REQUIRE(!e->isBranch());
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = DCPool<2>::build(a, Region<2>({-3, -3}, {-1, -1}), BRepSettings());
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing line")
    {
        auto e = DCPool<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}), BRepSettings());
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {1, 1}),
                                   BRepSettings());
        REQUIRE(!ta->isBranch());
    }

    SECTION("Containing shape")
    {
        auto t = DCPool<2>::build(circle(0.5), Region<2>({-1, -1}, {1, 1}),
                                  BRepSettings());
        REQUIRE(t->isBranch());
    }
}

TEST_CASE("DCTree<2>::rank()")
{
    SECTION("Containing line")
    {
        auto e = DCPool<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}),
                                  BRepSettings());
        REQUIRE(e->rank() == 1);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {1, 1}),
                                   BRepSettings());
        REQUIRE(ta->rank() == 2);
    }
}

TEST_CASE("DCTree<2>::vertex_count")
{
    BRepSettings settings;
    settings.min_feature = 100;

    SECTION("Single corner")
    {
        Tree a = min(Tree::X(), Tree::Y());

        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {3, 3}), settings);
        REQUIRE(ta->rank() == 2);
        REQUIRE(ta->level() == 0);
        REQUIRE(ta->leaf != nullptr);
        REQUIRE(ta->leaf->vertex_count == 1);
    }
    SECTION("Diagonally opposite corners")
    {
        Tree a = min(max(Tree::X(), Tree::Y()),
                     max(1 - Tree::X(), 1 - Tree::Y()));
        PointEvaluator eval(a);
        auto ta = DCPool<2>::build(a, Region<2>({-3, -3}, {3, 3}), settings);
        REQUIRE(ta->level() == 0);
        REQUIRE(ta->leaf != nullptr);
        REQUIRE(ta->leaf->vertex_count == 2);
        for (unsigned i=0; i < ta->leaf->vertex_count; ++i)
        {
            auto pt = ta->vert(i).template cast<float>().eval();
            CAPTURE(i);
            CAPTURE(pt.transpose());
            Eigen::Vector3f v;
            v << pt, 0.0f;
            REQUIRE(fabs(eval.eval(v)) < 1e-6);
        }
    }
}

TEST_CASE("DCTree<3>::vert")
{
    auto walk = [](Root<DCTree<3>>& xtree,
                   XTreeEvaluator& eval, float err)
    {
        std::list<const DCTree<3>*> todo = {xtree.get()};
        while (todo.size())
        {
            auto t = todo.front();
            todo.pop_front();
            if (t->isBranch())
            {
                for (auto& c : t->children)
                {
                    todo.push_back(c.load());
                }
            }
            if (!t->isBranch() && t->type == Interval::AMBIGUOUS)
            {
                for (unsigned i=0; i < t->leaf->vertex_count; ++i)
                {
                    REQUIRE(t->leaf != nullptr);

                    CAPTURE(t->vert(i).transpose());
                    CAPTURE(t->rank());
                    CAPTURE(t->level());
                    CAPTURE(t->leaf->vertex_count);
                    CAPTURE(i);
                    CAPTURE(t->leaf->manifold);
                    CAPTURE((int)t->leaf->corner_mask);
                    REQUIRE(eval.feature.eval(t->vert(i).template cast<float>())
                            == Approx(0.0f).margin(err));
                }
            }
        }
    };

    SECTION("Sliced box")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.3);
        XTreeEvaluator eval(b);
        Region<3> r({-2, -2, -2}, {2, 2, 2});

        BRepSettings settings;
        settings.min_feature = 0.1;
        auto xtree = DCPool<3>::build(b, r, settings);
        walk(xtree, eval, 0.001);
    }

    SECTION("Another sliced box")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.2);
        XTreeEvaluator eval(b);
        Region<3> r({-10, -10, -10}, {10, 10, 10});

        BRepSettings settings;
        settings.min_feature = 0.1;
        auto xtree = DCPool<3>::build(b, r, settings);
        walk(xtree, eval, 0.001);
    }

    SECTION("Sphere with circular cutout (manifoldness)")
    {
        auto s = max(sphere(1), -circle(0.5));
        Region<3> r({-5, -5, -5}, {5, 5, 5});

        BRepSettings settings;
        settings.min_feature = 1 / 9.0f;
        auto xtree = DCPool<3>::build(s, r, settings);
        XTreeEvaluator eval(s);
        walk(xtree, eval, 0.01);
    }

    SECTION("Nested rings with features on cell walls")
    {
        auto t = max(max(-Tree::Z(), max(circle(1), -circle(0.5))),
                          Tree::Z() - 1);
        Region<3> r({-2, -2, -2}, {2, 2, 2});

        BRepSettings settings;
        settings.min_feature = 0.2;
        auto xtree = DCPool<3>::build(t, r, settings);

        XTreeEvaluator eval(t);
        walk(xtree, eval, 0.01);
    }
}

TEST_CASE("DCTree<3> cancellation")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});
    XTreeEvaluator eval(sponge);

    BRepSettings settings;
    settings.min_feature = 0.02;
    settings.workers = 1;
    // Start a long render operation, then cancel it immediately
    auto future = std::async(std::launch::async, [&](){
        return DCPool<3>::build(&eval, r, settings); });

    // Record how long it takes betwen triggering the cancel
    // and the future finishing, so we can check that the cancelling
    // ended the computation within some short amount of time.
    start = std::chrono::system_clock::now();
    settings.cancel.store(true);
    auto result = future.get();
    end = std::chrono::system_clock::now();

    elapsed = end - start;
    auto elapsed_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);
    REQUIRE(elapsed_ms.count() < 150);

    // The cancelled computation must return nullptr
    // (rather than a partially-constructed or invalid tree)
    REQUIRE(result.get() == nullptr);
}

TEST_CASE("DCTree<3>: vertex sliding")
{
    auto c = rotate_x(-cylinder(1, 1, {0, 0, -0.5}), 0.4);
    auto r = Region<3>({-1.125, 0.375, 0.375}, {-0.75, 0.75, 0.75});

    BRepSettings settings;
    settings.min_feature = 0.5;
    settings.workers = 1;
    auto t = DCPool<3>::build(c, r, settings);

    REQUIRE(t->leaf != nullptr);
    REQUIRE(t->leaf->vertex_count == 1);

    CAPTURE(t->leaf->verts.col(0).transpose());
    CAPTURE(r.lower.transpose());
    CAPTURE(r.upper.transpose());
    REQUIRE(r.contains(t->leaf->verts.col(0), 0));
}
