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
}
