#include <catch/catch.hpp>

#include "region.hpp"

TEST_CASE("Region::DiscreteRange construction")
{
    auto da = Region::DiscreteRange(Interval(0, 1), 1);
    REQUIRE(da.size == 1);

    auto db = Region::DiscreteRange(Interval(0, 1), 10);
    REQUIRE(db.size == 10);

    auto dc = Region::DiscreteRange(Interval(0, 0), 1);
    REQUIRE(dc.size == 1);
}

TEST_CASE("Region::DiscreteRange pos")
{
    auto da = Region::DiscreteRange(Interval(0, 1), 1);
    REQUIRE(da.pos(0) == 0.5);

    auto db = Region::DiscreteRange(Interval(-0.5, 0.5), 3);
    REQUIRE(db.pos(1) == 0);
}

TEST_CASE("Region::DiscreteRange split")
{
    auto d = Region::DiscreteRange(Interval(0, 1), 10);
    auto ds = d.split();

    REQUIRE(ds.first.lower() == 0);
    REQUIRE(ds.first.upper() == 0.5);
    REQUIRE(ds.first.min == 0);
    REQUIRE(ds.first.size == 5);

    REQUIRE(ds.second.lower() == 0.5);
    REQUIRE(ds.second.upper() == 1.0);
    REQUIRE(ds.second.min == 5);
    REQUIRE(ds.second.size == 5);
}

TEST_CASE("Region resolution")
{
    auto r = Region({0, 10}, {0, 5}, {0, 2.5}, 10);

    REQUIRE(r.X.size == 100);
    REQUIRE(r.Y.size == 50);
    REQUIRE(r.Z.size == 25);
}

TEST_CASE("Region zero position")
{
    auto r = Region({0, 10}, {0, 5}, {0, 2.5}, 10);

    REQUIRE(r.X.min == 0);
    REQUIRE(r.Y.min == 0);
    REQUIRE(r.Z.min == 0);
}

TEST_CASE("Region canSplit")
{
    auto ra= Region({0, 1}, {0, 1}, {0, 2}, 1);
    REQUIRE(ra.canSplit());

    auto rb = Region({0, 1}, {0, 1}, {0, 1}, 1);
    REQUIRE(!rb.canSplit());
}

TEST_CASE("Splitting a region")
{
    auto r= Region({0, 1}, {0, 1}, {0, 2}, 1);
    auto rs = r.split();

    REQUIRE(rs.first.Z.min == 0);
    REQUIRE(rs.first.Z.size == 1);
    REQUIRE(rs.first.Z.lower() == 0);
    REQUIRE(rs.first.Z.upper() == 1);

    REQUIRE(rs.second.Z.min == 1);
    REQUIRE(rs.second.Z.size == 1);
    REQUIRE(rs.second.Z.lower() == 1);
    REQUIRE(rs.second.Z.upper() == 2);
}

TEST_CASE("Splitting a region with odd voxel count")
{
    auto r= Region({0, 1}, {0, 1}, {0, 3}, 1);
    auto rs = r.split();

    REQUIRE(rs.first.Z.min == 0);
    REQUIRE(rs.first.Z.size == 1);
    REQUIRE(rs.first.Z.lower() == 0);
    REQUIRE(rs.first.Z.upper() == 1);

    REQUIRE(rs.second.Z.min == 1);
    REQUIRE(rs.second.Z.size == 2);
    REQUIRE(rs.second.Z.lower() == 1);
    REQUIRE(rs.second.Z.upper() == 3);
}

TEST_CASE("Splitting a big region")
{
    Region r({-1, 1}, {-1, 1}, {0, 0}, 100);
    auto rs = r.split();

    REQUIRE(rs.first.X.lower() == -1);
    REQUIRE(rs.first.X.upper() == 0);

    REQUIRE(rs.second.X.lower() == 0);
    REQUIRE(rs.second.X.upper() == 1);
}
