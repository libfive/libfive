/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <future>

#include "catch.hpp"

#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/xtree_pool.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/deck.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>::vert")
{
    SECTION("Vertex positioning (with two planes)")
    {
        Tree a = min(Tree::X() + 0.1, Tree::Y() - 0.2);
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->vert().x() == Approx(-0.1));
        REQUIRE(ta->vert().y() == Approx(0.2));
    }
}

TEST_CASE("XTree<2>::type")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTreePool<2>::build(a, Region<2>({1, 1}, {2, 2}));
        REQUIRE(e->type == Interval::EMPTY);
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTreePool<2>::build(a, Region<2>({-3, -3}, {-1, -1}));
        REQUIRE(e->type == Interval::FILLED);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->type == Interval::AMBIGUOUS);
    }
}

TEST_CASE("XTree<2>::isBranch")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTreePool<2>::build(a, Region<2>({1, 1}, {2, 2}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTreePool<2>::build(a, Region<2>({-3, -3}, {-1, -1}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing line")
    {
        auto e = XTreePool<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(!ta->isBranch());
    }

    SECTION("Containing shape")
    {
        auto t = XTreePool<2>::build(circle(0.5), Region<2>({-1, -1}, {1, 1}));
        REQUIRE(t->isBranch());
    }
}

TEST_CASE("XTree<2>::rank()")
{
    SECTION("Containing line")
    {
        auto e = XTreePool<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}));
        REQUIRE(e->rank() == 1);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->rank() == 2);
    }
}

TEST_CASE("XTree<2>::vertex_count")
{
    SECTION("Single corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {3, 3}), 100);
        REQUIRE(ta->rank() == 2);
        REQUIRE(ta->level() == 0);
        REQUIRE(ta->leaf != nullptr);
        REQUIRE(ta->leaf->vertex_count == 1);
    }
    SECTION("Diagonally opposite corners")
    {
        Tree a = min(max(Tree::X(), Tree::Y()),
                     max(1 - Tree::X(), 1 - Tree::Y()));
        auto deck = std::make_shared<Deck>(a);
        PointEvaluator eval(deck);
        auto ta = XTreePool<2>::build(a, Region<2>({-3, -3}, {3, 3}), 100);
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

TEST_CASE("XTree<3>::vert")
{
    auto walk = [](XTree<3>::Root& xtree,
                   XTreeEvaluator& eval, float err=0.001)
    {
        std::list<const XTree<3>*> todo = {xtree.get()};
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
        auto xtree = XTreePool<3>::build(b, r, 0.1);
        walk(xtree, eval);
    }

    SECTION("Another sliced box")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.2);
        XTreeEvaluator eval(b);
        Region<3> r({-10, -10, -10}, {10, 10, 10});
        auto xtree = XTreePool<3>::build(b, r, 0.1);
        walk(xtree, eval);
    }

    SECTION("Sphere with circular cutout (manifoldness)")
    {
        auto s = max(sphere(1), -circle(0.5));
        Region<3> r({-5, -5, -5}, {5, 5, 5});
        auto xtree = XTreePool<3>::build(s, r, 1/9.0f);
        XTreeEvaluator eval(s);
        walk(xtree, eval, 0.01);
    }

    SECTION("Nested rings with features on cell walls")
    {
        auto t = max(max(-Tree::Z(), max(circle(1), -circle(0.5))),
                          Tree::Z() - 1);
        Region<3> r({-2, -2, -2}, {2, 2, 2});
        auto xtree = XTreePool<3>::build(t, r, 0.2);

        XTreeEvaluator eval(t);
        walk(xtree, eval, 0.01);
    }
}

TEST_CASE("XTree<3> cancellation")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});
    std::atomic_bool cancel(false);
    XTreeEvaluator eval(sponge);

    // Start a long render operation, then cancel it immediately
    auto future = std::async(std::launch::async, [&](){
        return XTreePool<3>::build(&eval, r, 0.02, 8, 1, cancel); });

    // Record how long it takes betwen triggering the cancel
    // and the future finishing, so we can check that the cancelling
    // ended the computation within some short amount of time.
    start = std::chrono::system_clock::now();
    cancel.store(true);
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
