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

#include "libfive/eval/feature.hpp"

using namespace Kernel;

TEST_CASE("Feature::push")
{
    SECTION("Pushing zero-length epsilon")
    {
        Feature f(Eigen::Vector3f::Zero());
        REQUIRE(f.push({0, 0, 0}) == false);
    }
    SECTION("Separated by exactly 180 degrees")
    {
        Feature f(Eigen::Vector3f::Zero());
        REQUIRE(f.push({1, 0, 0}) == true);
        REQUIRE(f.push({-1, 0, 0}) == false);
    }
    SECTION("Separability testing")
    {
        Feature a(Eigen::Vector3f::Zero());
        REQUIRE(a.push({1, 0, 0}) == true);
        REQUIRE(a.push({0, 1, 0}) == true);
        REQUIRE(a.push({0, 0, 1}) == true);
        REQUIRE(a.push({1, 1, 1}) == true);
        REQUIRE(a.push({-1, -1, -1}) == false);

        Feature b(Eigen::Vector3f::Zero());
        REQUIRE(b.push({1, 0, 0}) == true);
        REQUIRE(b.push({0, -1, 0}) == true);
        REQUIRE(b.push({0, 0, -1}) == true);
        REQUIRE(b.push({0, 0, -1}) == true);
        REQUIRE(b.push({0, -1, 0}) == true);
        REQUIRE(b.push({-1, 1, 1}) == false);
        REQUIRE(b.push({1, -1, -1}) == true);
    }

    SECTION("Fun with numerical instability")
    {
        Feature a(Eigen::Vector3f::Zero());
        auto s = sqrt(2);
        REQUIRE(a.push({s,  0, -s}) == true);
        REQUIRE(a.push({s,  0,  s}) == true);
        REQUIRE(a.push({1, 0, -1}) == true);
    }
}

TEST_CASE("Feature::check")
{
    SECTION("Flat plane")
    {
        Feature a(Eigen::Vector3f::Zero());
        REQUIRE(a.push({-1, 1, 0}) == true);
        REQUIRE(a.push({-1, -1, 0}) == true);
        REQUIRE(a.check({0, -1, 0}));
        REQUIRE(a.check({0, 1, 0}));

        REQUIRE(a.push({0, 1, 0}));
        REQUIRE(!a.check({0, -1, 0}));
    }
}

TEST_CASE("Feature::check(Feature)")
{
    SECTION("Tetrahedron")
    {
        Feature a(Eigen::Vector3f::Zero());
        REQUIRE(a.push({0, 0,  1}) == true);
        REQUIRE(a.push({1, 0, -1}) == true);

        Feature b(Eigen::Vector3f::Zero());
        REQUIRE(b.push({-1,  1, -1}) == true);
        REQUIRE(b.push({-1, -1, -1}) == true);

        REQUIRE(!a.check(b));
    }
}
