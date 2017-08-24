#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>::vert")
{
    SECTION("Vertex positioning (with two planes)")
    {
        Tree a = min(Tree::X() + 0.1, Tree::Y() - 0.2);
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->vert().x() == Approx(-0.1));
        REQUIRE(ta->vert().y() == Approx(0.2));
    }
}

TEST_CASE("XTree<2>::type")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTree<2>::build(a, Region<2>({1, 1}, {2, 2}));
        REQUIRE(e->type == Interval::EMPTY);
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTree<2>::build(a, Region<2>({-3, -3}, {-1, -1}));
        REQUIRE(e->type == Interval::FILLED);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->type == Interval::AMBIGUOUS);
    }
}

TEST_CASE("XTree<2>::isBranch")
{
    SECTION("Empty")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTree<2>::build(a, Region<2>({1, 1}, {2, 2}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Filled")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto e = XTree<2>::build(a, Region<2>({-3, -3}, {-1, -1}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing line")
    {
        auto e = XTree<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}));
        REQUIRE(!e->isBranch());
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(!ta->isBranch());
    }

    SECTION("Containing shape")
    {
        Evaluator e = circle(0.5);
        auto t = XTree<2>::build(circle(0.5), Region<2>({-1, -1}, {1, 1}));
        REQUIRE(t->isBranch());
    }
}

TEST_CASE("XTree<2>::rank")
{
    SECTION("Containing line")
    {
        auto e = XTree<2>::build(Tree::X(), Region<2>({-2, -2}, {2, 2}));
        REQUIRE(e->rank == 1);
    }

    SECTION("Containing corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->rank == 2);
    }
}

TEST_CASE("XTree<2>::vertex_count")
{
    SECTION("Single corner")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {3, 3}), 100);
        REQUIRE(ta->rank == 2);
        REQUIRE(ta->level == 0);
        REQUIRE(ta->vertex_count == 1);
    }
    SECTION("Diagonally opposite corners")
    {
        Tree a = min(max(Tree::X(), Tree::Y()),
                     max(1 - Tree::X(), 1 - Tree::Y()));
        Evaluator eval(a);
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {3, 3}), 100);
        REQUIRE(ta->level == 0);
        REQUIRE(ta->vertex_count == 2);
        for (unsigned i=0; i < ta->vertex_count; ++i)
        {
            auto pt = ta->vert3(i).template cast<float>().eval();
            CAPTURE(i);
            CAPTURE(pt.transpose());
            REQUIRE(fabs(eval.eval(pt)) < 1e-6);
        }
    }
}

TEST_CASE("XTree<3>::vert")
{
    auto walk = [](std::unique_ptr<const XTree<3>>& xtree,
                   Evaluator& eval, float err=0.001)
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
                    todo.push_back(c.get());
                }
            }
            if (!t->isBranch() && t->type == Interval::AMBIGUOUS)
            {
                for (unsigned i=0; i < t->vertex_count; ++i)
                {
                    CAPTURE(t->vert(i).transpose());
                    CAPTURE(t->rank);
                    CAPTURE(t->level);
                    CAPTURE(t->vertex_count);
                    CAPTURE(i);
                    CAPTURE(t->manifold);
                    CAPTURE((int)t->corner_mask);
                    CAPTURE(t->region.lower.transpose());
                    CAPTURE(t->region.upper.transpose());
                    REQUIRE(eval.eval(t->vert3(i).template cast<float>())
                            == Approx(0).epsilon(err));
                }
            }
        }
    };

    SECTION("Sliced box")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.3);
        Evaluator eval(b);
        Region<3> r({-2, -2, -2}, {2, 2, 2});
        auto xtree = XTree<3>::build(b, r, 0.1);
        walk(xtree, eval);
    }

    SECTION("Another sliced box")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.2);
        Evaluator eval(b);
        Region<3> r({-10, -10, -10}, {10, 10, 10});
        auto xtree = XTree<3>::build(b, r, 0.1);
        walk(xtree, eval);
    }

    SECTION("Sphere with circular cutout (manifoldness)")
    {
        auto s = max(sphere(1), -circle(0.5));
        Region<3> r({-5, -5, -5}, {5, 5, 5});
        auto xtree = XTree<3>::build(s, r, 1/9.0f);
        Evaluator eval(s);
        walk(xtree, eval, 0.01);
    }
}
