#include <Eigen/Geometry>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/tape.hpp"

using namespace Kernel;

TEST_CASE("Tape::num_clauses")
{
    Tape t(Tree::X() + 1);
    REQUIRE(t.num_clauses == 5); // X, Y, Z, 1, +
}

TEST_CASE("Tape::XYZ")
{
    Tape t(Tree::X() + 1);
    REQUIRE(t.X == 3);
    REQUIRE(t.Y == 4);
    REQUIRE(t.Z == 5);
}

TEST_CASE("Tape::constants")
{
    Tape t(Tree::X() + 5);
    REQUIRE(t.constants.size() == 1);

    CAPTURE(t.constants.begin()->first);
    CAPTURE(t.constants.begin()->second);
    REQUIRE(t.constants.at(2) == 5.0f);
}
