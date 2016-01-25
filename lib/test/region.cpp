#include <catch/catch.hpp>

#include "ao/render/region.hpp"

TEST_CASE("Region::Axis construction")
{
    auto da = Region::Axis(Interval(0, 1), 1);
    REQUIRE(da.values.size() == 1);

    auto db = Region::Axis(Interval(0, 1), 10);
    REQUIRE(db.values.size() == 10);

    auto dc = Region::Axis(Interval(0, 0), 1);
    REQUIRE(dc.values.size() == 1);
}

TEST_CASE("Region::Axis values")
{
    auto da = Region::Axis(Interval(0, 1), 1);
    REQUIRE(da.values[0] == 0.5);

    auto db = Region::Axis(Interval(-0.5, 0.5), 3);
    REQUIRE(db.values[1] == 0);
}

TEST_CASE("Region resolution")
{
    auto r = Region({0, 10}, {0, 5}, {0, 2.5}, 10);

    REQUIRE(r.X.values.size() == 100);
    REQUIRE(r.Y.values.size() == 50);
    REQUIRE(r.Z.values.size() == 25);
}
