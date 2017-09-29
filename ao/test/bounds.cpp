#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/solve/bounds.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("findBounds")
{
    SECTION("Simple sphere")
    {
        auto s = sphere(0.5);
        auto r = findBounds(s);
        CAPTURE(r.lower);
        CAPTURE(r.upper);
        REQUIRE(r.lower.x() == Approx(-0.5).epsilon(0.001));
        REQUIRE(r.lower.y() == Approx(-0.5).epsilon(0.001));
        REQUIRE(r.lower.z() == Approx(-0.5).epsilon(0.001));

        REQUIRE(r.upper.x() == Approx(0.5).epsilon(0.001));
        REQUIRE(r.upper.y() == Approx(0.5).epsilon(0.001));
        REQUIRE(r.upper.z() == Approx(0.5).epsilon(0.001));
    }
}
