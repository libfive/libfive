#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>()")
{
    SECTION("Vertex positioning (with two planes)")
    {
        Evaluator a(min(Tree::X(), -Tree::X() + 0.2));
        auto ta = XTree<2>(&a, Region<2>({-1, -1}, {1, 1}));
        REQUIRE(ta.vert.x() == Approx(0.1));
        REQUIRE(ta.vert.y() == Approx(0.0));

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
        auto ta = XTree<2>(&a, Region<2>({0.6, -0.2}, {0.8, 0.2}));
        REQUIRE(ta.vert.x() == Approx(0.6).epsilon(0.01));
        REQUIRE(ta.vert.y() == Approx(0.0).epsilon(0.01));
    }

    SECTION("Snapping to cell corners (curved)")
    {
        Evaluator a(circle(0.5));
        {
            auto ta = XTree<2>(&a, Region<2>({0.6, 0.6}, {0.8, 0.8}), 1000);
            REQUIRE(ta.vert.x() == Approx(0.6).epsilon(0.01));
            REQUIRE(ta.vert.y() == Approx(0.6).epsilon(0.01));
        }

        {
            auto ta = XTree<2>(&a, Region<2>({-1, -1}, {0, 0}), 1000);
            REQUIRE(ta.vert.x() == Approx(0).epsilon(0.01));
            REQUIRE(ta.vert.y() == Approx(0).epsilon(0.01));
        }
    }
}
