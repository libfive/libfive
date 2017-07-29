#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>::vert")
{
    SECTION("Vertex positioning (with two planes)")
    {
        Tree a = min(Tree::X(), Tree::Y());
        auto ta = XTree<2>::build(a, Region<2>({-3, -3}, {1, 1}));
        REQUIRE(ta->vert.x() == Approx(0.0));
        REQUIRE(ta->vert.y() == Approx(0.0));
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

TEST_CASE("XTree<3>::vert")
{
    SECTION("Mesh::Render (sliced box)")
    {
        auto b = max(box({0, 0, 0}, {1, 1, 1}),
                Tree::X() + Tree::Y() + Tree::Z() - 1.3);
        Evaluator eval(b);
        Region<3> r({-2, -2, -2}, {2, 2, 2});
        auto xtree = XTree<3>::build(b, r, 0.1);

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
                CAPTURE(t->vert.transpose());
                REQUIRE(eval.eval(t->vert.template cast<float>()) == Approx(0).epsilon(0.05));
            }
        }
    }
}

/*
        Evaluator b(max(Tree::X(), -Tree::X() + 0.2));
        auto tb = XTree<2>(&a, Region<2>({-1, -1}, {1, 1}));
        REQUIRE(tb.vert.x() == Approx(0.1));
        REQUIRE(tb.vert.y() == Approx(0.0));
    }

    SECTION("Snapping to cell center")
    {
        Evaluator a(Tree::X());
        auto ta = XTree<2>(&a, Region<2>({10, 13}, {12, 15}));
        REQUIRE(ta.vert.x() == Approx(11));
        REQUIRE(ta.vert.y() == Approx(14));
    }

    SECTION("Snapping to cell walls (curved)")
    {
        Evaluator a(circle(0.5));
        auto ta = XTree<2>(&a, Region<2>({0.6, -0.2}, {0.8, 0.2}), 1000);
        CAPTURE(ta.vert.transpose());
        REQUIRE(ta.vert.x() == Approx(0.6).epsilon(0.01));
        REQUIRE(ta.vert.y() == Approx(0.0).epsilon(0.01));
    }

    SECTION("Snapping to cell corners (curved)")
    {
        Evaluator a(circle(0.5));
        {
            auto t = XTree<2>(&a, Region<2>({0.6, 0.6}, {0.8, 0.8}), 1000);
            REQUIRE(t.vert.x() == Approx(0.6).epsilon(0.01));
            REQUIRE(t.vert.y() == Approx(0.6).epsilon(0.01));
        }

        {
            auto t = XTree<2>(&a, Region<2>({-1, -1}, {0, 0}), 1000);
            REQUIRE(t.vert.x() == Approx(0).epsilon(0.01));
            REQUIRE(t.vert.y() == Approx(0).epsilon(0.01));
        }

        {
            auto ta = XTree<2>(&a, Region<2>({-1, -2}, {0, -1}), 1000);
            auto tb = XTree<2>(&a, Region<2>({-2, -1}, {-1, 0}), 1000);
            CAPTURE(ta.vert.transpose());
            CAPTURE(tb.vert.transpose());
            REQUIRE(ta.vert.y() == Approx(-1));
            REQUIRE(ta.vert.x() == Approx(tb.vert.y()));
            REQUIRE(ta.vert.y() == Approx(tb.vert.x()));
        }
    }

}
    */

/*
TEST_CASE("XTree<2>(Scaffold)")
{
    SECTION("Rectangle corners")
    {
        auto rect = rectangle(-1, 1, -1, 1);
        Region<2> r({-1, -1}, {1, 1});
        auto a = Evaluator(rect);

        // Make padded scaffold
        auto s = Scaffold<2>(&a, r, 2, true);

        // Build a tree onto the scaffold
        auto ta = XTree<2>(&a, s);

        // We want to check that each corner has a vertex on it
        std::array<Eigen::Array2f, 4> corners;
        corners[0] = {-1, -1};
        corners[1] = {1, -1};
        corners[2] = {-1, 1};
        corners[3] = {1, 1};
        std::array<float, 4> scores = {{1, 1, 1, 1}};

        // Iterate over all cells, checking vertex positions
        std::list<XTree<2>*> targets = {&ta};
        while (targets.size())
        {
            auto t = targets.front();
            targets.pop_front();

            for (unsigned i=0; i < 4; ++i)
            {
                scores[i] = fmin(scores[i], (t->vert - corners[i]).matrix().squaredNorm());
            }
            for (auto& n : t->children)
            {
                if (n)
                {
                    targets.push_back(n.get());
                }
            }
        }

        for (unsigned i=0; i < 4; ++i)
        {
            REQUIRE(scores[i] < 1e-4);
        }
    }
}
*/
