#include <catch/catch.hpp>

#include "kernel/eval/feature.hpp"

using namespace Kernel;

TEST_CASE("Feature::push")
{
    SECTION("Pushing zero-length epsilon")
    {
        Feature f;
        REQUIRE(f.push({0, 0, 0}) == false);
    }
    SECTION("Separated by exactly 180 degrees")
    {
        Feature f;
        REQUIRE(f.push({1, 0, 0}) == true);
        REQUIRE(f.push({-1, 0, 0}) == false);
    }
    SECTION("Separability testing")
    {
        Feature a;
        REQUIRE(a.push({1, 0, 0}) == true);
        REQUIRE(a.push({0, 1, 0}) == true);
        REQUIRE(a.push({0, 0, 1}) == true);
        REQUIRE(a.push({1, 1, 1}) == true);
        REQUIRE(a.push({-1, -1, -1}) == false);

        Feature b;
        REQUIRE(b.push({1, 0, 0}) == true);
        REQUIRE(b.push({0, -1, 0}) == true);
        REQUIRE(b.push({0, 0, -1}) == true);
        REQUIRE(b.push({0, 0, -1}) == true);
        REQUIRE(b.push({0, -1, 0}) == true);
        REQUIRE(b.push({-1, 1, 1}) == false);
        REQUIRE(b.push({1, -1, -1}) == true);
    }
}

TEST_CASE("Feature::isCompatible")
{
    SECTION("Flat plane")
    {
        Feature a;
        REQUIRE(a.push({-1, 1, 0}) == true);
        REQUIRE(a.push({-1, -1, 0}) == true);
        REQUIRE(a.isCompatible({0, -1, 0}));
        REQUIRE(a.isCompatible({0, 1, 0}));

        REQUIRE(a.push({0, 1, 0}));
        REQUIRE(!a.isCompatible({0, -1, 0}));
    }
}
