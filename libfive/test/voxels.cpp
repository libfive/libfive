/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "catch.hpp"

#include "libfive/render/discrete/voxels.hpp"

using namespace Kernel;

TEST_CASE("Voxels::pts values")
{
    SECTION("Exact values")
    {
        auto a = Voxels({0, 0, 0}, {1, 1, 1}, 1.0f);
        REQUIRE(a.pts[0][0] == 0.5);

        auto b = Voxels({-0.5, 0, 0}, {0.5, 1, 1}, 3.0f);
        REQUIRE(b.pts[0][1] == 0);

        auto c = Voxels({-1, 0, 0}, {1, 1, 1}, 0.0f);
        REQUIRE(c.pts[0][0] == 0);
    }

    SECTION("Expanding interval")
    {
        auto a = Voxels({0, 0, 0}, {1.2f, 1.2f, 1.2f}, 1);
        REQUIRE(a.lower.x() == Approx(-0.4));
        REQUIRE(a.upper.x() == Approx(1.6));
        REQUIRE(a.pts[0][0] == Approx(0.1f));
        REQUIRE(a.pts[0][1] == Approx(1.1f));
    }
}

TEST_CASE("Voxels::pts size")
{
    SECTION("Simple")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 2}, 1);
        REQUIRE(r.pts[0].size() == 1);
        REQUIRE(r.pts[1].size() == 1);
        REQUIRE(r.pts[2].size() == 2);
    }

    SECTION("Exact values")
    {
        auto r = Voxels({0, 0, 0}, {10, 5, 2.5}, 10);

        REQUIRE(r.pts[0].size() == 100);
        REQUIRE(r.pts[1].size() == 50);
        REQUIRE(r.pts[2].size() == 25);
    }

    SECTION("Expanding interval")
    {
        auto r = Voxels({0, 0, 0}, {1.1f, 2.1f, 3.1f}, 1.0f);
        REQUIRE(r.pts[0].size() == 2);
        REQUIRE(r.pts[1].size() == 3);
        REQUIRE(r.pts[2].size() == 4);
    }

    SECTION("Per-axis resolution")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 1}, {1, 2, 3});
        REQUIRE(r.pts[0].size() == 1);
        REQUIRE(r.pts[1].size() == 2);
        REQUIRE(r.pts[2].size() == 3);
    }

    SECTION("Empty axes")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 1}, {3, 2, 0});
        REQUIRE(r.pts[0].size() == 3);
        REQUIRE(r.pts[1].size() == 2);
        REQUIRE(r.pts[2].size() == 1);
    }
}

TEST_CASE("Voxels::View::lower")
{
    auto r = Voxels({0, 0, 0}, {10, 5, 2.5}, 10);
    auto v = r.view();
    REQUIRE(v.lower == Eigen::Vector3f(0, 0, 0));
}

TEST_CASE("Voxels::View::size")
{
    auto r = Voxels({0, 0, 0}, {1, 1, 2}, 1);
    auto v = r.view();
    REQUIRE(v.size == Eigen::Vector3i(1, 1, 2));
}

TEST_CASE("Voxels::View::split")
{
    SECTION("Simple")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 1}, 10);
        auto v = r.view();

        auto vs = v.split();
        auto a = vs.first;
        auto b = vs.second;

        REQUIRE(a.lower.x() == 0);
        REQUIRE(a.upper.x() == 0.5);
        REQUIRE(a.corner.x() == 0);
        REQUIRE(a.size.x() == 5);

        REQUIRE(b.lower.x() == 0.5);
        REQUIRE(b.upper.x() == 1);
        REQUIRE(b.corner.x() == 5);
        REQUIRE(b.size.x() == 5);

        REQUIRE(a.lower.tail<2>() == v.lower.tail<2>());
        REQUIRE(a.upper.tail<2>() == v.upper.tail<2>());
        REQUIRE(a.corner.tail<2>() == v.corner.tail<2>());
        REQUIRE(a.size.tail<2>() == v.size.tail<2>());

        REQUIRE(b.lower.tail<2>() == v.lower.tail<2>());
        REQUIRE(b.upper.tail<2>() == v.upper.tail<2>());
        REQUIRE(b.corner.tail<2>() == v.corner.tail<2>());
        REQUIRE(b.size.tail<2>() == v.size.tail<2>());
    }

    SECTION("Axis selection")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 2}, 1);
        auto v = r.view();
        REQUIRE(v.size.z() == 2);

        auto vs = v.split();
        auto a = vs.first;
        auto b = vs.second;

        // Check that X and Y axes haven't changed
        REQUIRE(a.lower.head<2>() == v.lower.head<2>());
        REQUIRE(a.upper.head<2>() == v.upper.head<2>());
        REQUIRE(a.corner.head<2>() == v.corner.head<2>());
        REQUIRE(a.size.head<2>() == v.size.head<2>());

        CAPTURE(b.lower);
        CAPTURE(b.upper);
        CAPTURE(b.corner);
        CAPTURE(b.size);
        REQUIRE(b.lower.head<2>() == v.lower.head<2>());
        REQUIRE(b.upper.head<2>() == v.upper.head<2>());
        REQUIRE(b.corner.head<2>() == v.corner.head<2>());
        REQUIRE(b.size.head<2>() == v.size.head<2>());

        // Confirm that Z axis was correctly split
        REQUIRE(a.lower.z() == 0);
        REQUIRE(a.upper.z() == 1);
        REQUIRE(a.corner.z() == 0);
        REQUIRE(a.size.z() == 1);

        REQUIRE(b.lower.z() == 1);
        REQUIRE(b.upper.z() == 2);
        REQUIRE(b.corner.z() == 1);
        REQUIRE(b.size.z() == 1);
    }

    SECTION("With odd voxel count")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 3}, 1);
        auto v = r.view();

        auto vs = v.split();
        auto a = vs.first;
        auto b = vs.second;

        // Check that X and Y axes haven't changed
        REQUIRE(a.lower.head<2>() == v.lower.head<2>());
        REQUIRE(a.upper.head<2>() == v.upper.head<2>());
        REQUIRE(a.corner.head<2>() == v.corner.head<2>());
        REQUIRE(a.size.head<2>() == v.size.head<2>());

        REQUIRE(b.lower.head<2>() == v.lower.head<2>());
        REQUIRE(b.upper.head<2>() == v.upper.head<2>());
        REQUIRE(b.corner.head<2>() == v.corner.head<2>());
        REQUIRE(b.size.head<2>() == v.size.head<2>());

        // Confirm that Z axis was correctly split
        REQUIRE(a.lower.z() == 0);
        REQUIRE(a.upper.z() == 2);
        REQUIRE(a.corner.z() == 0);
        REQUIRE(a.size.z() == 2);

        REQUIRE(b.lower.z() == 2);
        REQUIRE(b.upper.z() == 3);
        REQUIRE(b.corner.z() == 2);
        REQUIRE(b.size.z() == 1);
    }

    SECTION("Splitting a unit voxel")
    {
        auto r = Voxels({0, 0, 0}, {1, 1, 1}, 1);
        auto v = r.view();

        auto vs = v.split();
        auto a = vs.first;
        auto b = vs.second;

        REQUIRE(!a.empty());
        REQUIRE(b.empty());
    }
}
