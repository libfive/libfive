#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>()")
{
    Evaluator e(min(Tree::X(), -Tree::X() + 0.2));

    auto t = XTree<2>(&e, Region<2>({-1, -1}, {1, 1}));
    REQUIRE(true);
}
