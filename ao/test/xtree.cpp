#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"

using namespace Kernel;

TEST_CASE("XTree<2>()")
{
    Evaluator e(Tree::X());

    auto t = XTree<2>(&e, Region<2>({-1, -1}, {1, 1}));
    REQUIRE(true);
}
