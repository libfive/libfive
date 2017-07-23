#include "catch.hpp"

#include "ao/render/brep/scaffold.hpp"

using namespace Kernel;

TEST_CASE("Scaffold<2>::Scaffold")
{
    Evaluator e(Tree::X());
    Region<2> r({-1, -1}, {1, 1});

    auto s = Scaffold<2>(&e, r, 2);

    REQUIRE(s.children[0].get() != nullptr);
    REQUIRE(s.children[0]->type == Interval::AMBIGUOUS);
    REQUIRE(s.children[0]->children[0]->type == Interval::FILLED);
    REQUIRE(s.children[1]->children[0]->type == Interval::AMBIGUOUS);
    REQUIRE(s.children[1]->children[1]->type == Interval::EMPTY);
}

TEST_CASE("Scaffold<2>::Scaffold (pad=true)")
{
    Evaluator e(Tree::X());
    Region<2> r({-1, -1}, {1, 1}, Region<2>::Perp(1.5));

    auto s = Scaffold<2>(&e, r, 2, true);

    CAPTURE(s.region.perp);
    REQUIRE((s.region.perp == Region<2>::Perp(1.5)).all());
}

TEST_CASE("Scaffold<3>::Scaffold (pad=true)")
{
    Evaluator e(Tree::X());
    Region<3> r({-1, -1, -1}, {1, 1, 1});

    SECTION("depth=2")
    {
        auto s = Scaffold<3>(&e, r, 2, true);

        CAPTURE(s.region.lower);
        REQUIRE((s.region.lower == Eigen::Array3f(-2, -2, -2)).all());
        CAPTURE(s.region.upper);
        REQUIRE((s.region.upper == Eigen::Array3f(2, 2, 2)).all());

        REQUIRE(s.children[0].get());
        REQUIRE(s.children[0]->children[0]);
        REQUIRE(!s.children[0]->children[0]->children[0].get());
    }
}
