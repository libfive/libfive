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
    SECTION("Simple")
    {
        auto r = Region({0, 0, 0}, {1, 1, 2}, 1);
        REQUIRE(r.size.x() == 1);
        REQUIRE(r.size.y() == 1);
        REQUIRE(r.size.z() == 2);
    }

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

TEST_CASE("Region::View::lower")
{
    auto r = Region({0, 0, 0}, {10, 5, 2.5}, 10);
    auto v = r.view();
    REQUIRE(v.lower == Eigen::Vector3f(0, 0, 0));
}

TEST_CASE("Region::View::size")
{
    auto r = Region({0, 0, 0}, {1, 1, 2}, 1);
    auto v = r.view();
    REQUIRE(v.size == Eigen::Vector3i(1, 1, 2));
}

TEST_CASE("Region::View::split")
{
    SECTION("Simple")
    {
        auto r = Region({0, 0, 0}, {1, 1, 1}, 10);
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
    }

    SECTION("Axis selection")
    {
        auto r = Region({0, 0, 0}, {1, 1, 2}, 1);
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
        auto r = Region({0, 0, 0}, {1, 1, 3}, 1);
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
}
