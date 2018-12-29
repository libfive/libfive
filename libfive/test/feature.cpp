/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/eval/feature.hpp"

using namespace Kernel;

TEST_CASE("Feature::push")
{
    SECTION("Pushing zero-length epsilon")
    {
        Feature f(Eigen::Vector3d::Zero());
        REQUIRE(f.push({0, 0, 0}) == false);
    }
    SECTION("Separated by exactly 180 degrees")
    {
        Feature f(Eigen::Vector3d::Zero());
        REQUIRE(f.push({1, 0, 0}) == true);
        REQUIRE(f.push({-1, 0, 0}) == false);
    }
    SECTION("Separability testing")
    {
        Feature a(Eigen::Vector3d::Zero());
        REQUIRE(a.push({1, 0, 0}) == true);
        REQUIRE(a.push({0, 1, 0}) == true);
        REQUIRE(a.push({0, 0, 1}) == true);
        REQUIRE(a.push({1, 1, 1}) == true);
        REQUIRE(a.push({-1, -1, -1}) == false);

        Feature b(Eigen::Vector3d::Zero());
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
        Feature a(Eigen::Vector3d::Zero());
        auto s = sqrt(2.f);
        REQUIRE(a.push({s,  0, -s}) == true);
        REQUIRE(a.push({s,  0,  s}) == true);
        REQUIRE(a.push({1, 0, -1}) == true);
    }
}

TEST_CASE("Feature::check")
{
    SECTION("Flat plane")
    {
        Feature a(Eigen::Vector3d::Zero());
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
        Feature a(Eigen::Vector3d::Zero());
        REQUIRE(a.push({0, 0,  1}) == true);
        REQUIRE(a.push({1, 0, -1}) == true);

        Feature b(Eigen::Vector3d::Zero());
        REQUIRE(b.push({-1,  1, -1}) == true);
        REQUIRE(b.push({-1, -1, -1}) == true);

        REQUIRE(!a.check(b));
    }
}
