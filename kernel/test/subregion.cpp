/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <catch/catch.hpp>

#include "ao/kernel/render/region.hpp"
#include "ao/kernel/render/subregion.hpp"

TEST_CASE("Subregion::Axis split")
{
    auto d = Region::Axis(Interval(0, 1), 10.0f);
    auto s = Subregion::Axis(d.bounds, d.values);
    auto ds = s.split();

    REQUIRE(ds.first.lower() == 0);
    REQUIRE(ds.first.upper() == 0.5);
    REQUIRE(ds.first.min == 0);
    REQUIRE(ds.first.size == 5);

    REQUIRE(ds.second.lower() == 0.5);
    REQUIRE(ds.second.upper() == 1.0);
    REQUIRE(ds.second.min == 5);
    REQUIRE(ds.second.size == 5);
}

TEST_CASE("Subregion zero position")
{
    auto r = Region({0, 10}, {0, 5}, {0, 2.5}, 10);
    auto s = r.view();

    REQUIRE(s.X.min == 0);
    REQUIRE(s.Y.min == 0);
    REQUIRE(s.Z.min == 0);
}

TEST_CASE("Subregion canSplit")
{
    auto ra= Region({0, 1}, {0, 1}, {0, 2}, 1);
    auto sa = ra.view();
    REQUIRE(sa.canSplit());

    auto rb = Region({0, 1}, {0, 1}, {0, 1}, 1);
    auto sb = rb.view();
    REQUIRE(!sb.canSplit());
}

TEST_CASE("Splitting a region")
{
    auto r = Region({0, 1}, {0, 1}, {0, 2}, 1);
    auto s = r.view();
    auto rs = s.split();

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
    auto r = Region({0, 1}, {0, 1}, {0, 3}, 1);
    auto s = r.view();
    auto rs = s.split();

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
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.X.lower() == -1);
    REQUIRE(rs.first.X.upper() == 0);

    REQUIRE(rs.second.X.lower() == 0);
    REQUIRE(rs.second.X.upper() == 1);
}

TEST_CASE("Splitting a region with equal voxels")
{
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 8);
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.X.lower() == -1);
    REQUIRE(rs.first.X.upper() == 0);
}
