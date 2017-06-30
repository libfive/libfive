#include "catch.hpp"

#include "ao/render/region.hpp"

using namespace Kernel;

TEST_CASE("Region::XYZ")
{
    SECTION("Exact values")
    {
        auto a = Region({0, 0, 0}, {1, 1, 1}, 1.0f);
        REQUIRE(a.pts[0][0] == 0.5);

        auto b = Region({-0.5, 0, 0}, {0.5, 1, 1}, 3.0f);
        REQUIRE(b.pts[0][1] == 0);

        auto c = Region({-1, 0, 0}, {1, 1, 1}, 0.0f);
        REQUIRE(c.pts[0][0] == 0);
    }

    SECTION("Expanding interval")
    {
        auto a = Region({0, 0, 0}, {1.2, 1.2, 1.2}, 1);
        REQUIRE(a.lower.x() == Approx(-0.4));
        REQUIRE(a.upper.x() == Approx(1.6));
        REQUIRE(a.pts[0][0] == Approx(0.1f));
        REQUIRE(a.pts[0][1] == Approx(1.1f));
    }
}

TEST_CASE("Region::size")
{
    SECTION("Exact values")
    {
        auto r = Region({0, 0, 0}, {10, 5, 2.5}, 10);

        REQUIRE(r.size.x() == 100);
        REQUIRE(r.size.y() == 50);
        REQUIRE(r.size.z() == 25);
    }

    SECTION("Expanding interval")
    {
        auto r = Region({0, 0, 0}, {1.1, 2.1, 3.1}, 1.0f);
        REQUIRE(r.size.x() == 2);
        REQUIRE(r.size.y() == 3);
        REQUIRE(r.size.z() == 4);
    }

    SECTION("Per-axis resolution")
    {
        auto r = Region({0, 0, 0}, {1, 1, 1}, {1, 2, 3});
        REQUIRE(r.size.x() == 1);
        REQUIRE(r.size.y() == 2);
        REQUIRE(r.size.z() == 3);
    }
}
