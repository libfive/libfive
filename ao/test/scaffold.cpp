#include "catch.hpp"

#include "ao/render/brep/scaffold.hpp"

using namespace Kernel;

TEST_CASE("Scaffold<2>::Scaffold")
{
    Evaluator e(Tree::X());
    Region<2> r({-1, -1}, {1, 1});

    auto s = Scaffold<2>(&e, r, 2);

    REQUIRE(s.children[0].get() != nullptr);
    REQUIRE(s.children[0]->type == s.AMBIGUOUS);
    REQUIRE(s.children[0]->children[0]->type == s.FULL);
    REQUIRE(s.children[1]->children[0]->type == s.AMBIGUOUS);
    REQUIRE(s.children[1]->children[1]->type == s.EMPTY);
}
