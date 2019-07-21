/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/region.hpp"
#include "libfive/render/axes.hpp"

using namespace libfive;

TEST_CASE("Region<2>::subdivide")
{
    SECTION("On all axes")
    {
        Region<2> a({-1, -2}, {1, 2});
        auto out = a.subdivide();
        REQUIRE(out.size() == 4);

        for (int i=0; i < 4; ++i)
        {
            const auto& sub = out[i];
            CAPTURE(sub.lower.x());
            CAPTURE(sub.upper.x());
            CAPTURE(sub.lower.y());
            CAPTURE(sub.upper.y());
            if (i & Axis::X)
            {
                REQUIRE(sub.lower.x() ==  0);
                REQUIRE(sub.upper.x() ==  1);
            }
            else
            {
                REQUIRE(sub.lower.x() == -1);
                REQUIRE(sub.upper.x() ==  0);
            }

            if (i & Axis::Y)
            {
                REQUIRE(sub.lower.y() ==  0);
                REQUIRE(sub.upper.y() ==  2);
            }
            else
            {
                REQUIRE(sub.lower.y() == -2);
                REQUIRE(sub.upper.y() ==  0);
            }

            REQUIRE(!(i & Axis::Z));
        }
    }

    SECTION("With perp")
    {
        Region<2> a({-1, -2}, {1, 2}, Region<2>::Perp(10));
        auto out = a.subdivide();

        for (int i=0; i < 4; ++i)
        {
            CAPTURE(out[i].perp);
            REQUIRE((out[i].perp == Region<2>::Perp(10)).all());
        }
    }
}

TEST_CASE("Region<3>::subdivide")
{
    Region<3> a({-1, -2, -4}, {1, 2, 4});

    SECTION("On all axes")
    {
        auto out = a.subdivide();

        REQUIRE(out.size() == 8);
        for (int i=0; i < 8; ++i)
        {
            const auto& sub = out[i];
            CAPTURE(sub.lower.x());
            CAPTURE(sub.upper.x());
            CAPTURE(sub.lower.y());
            CAPTURE(sub.upper.y());
            CAPTURE(sub.lower.z());
            CAPTURE(sub.upper.z());
            if (i & Axis::X)
            {
                REQUIRE(sub.lower.x() ==  0);
                REQUIRE(sub.upper.x() ==  1);
            }
            else
            {
                REQUIRE(sub.lower.x() == -1);
                REQUIRE(sub.upper.x() ==  0);
            }

            if (i & Axis::Y)
            {
                REQUIRE(sub.lower.y() ==  0);
                REQUIRE(sub.upper.y() ==  2);
            }
            else
            {
                REQUIRE(sub.lower.y() == -2);
                REQUIRE(sub.upper.y() ==  0);
            }

            if (i & Axis::Z)
            {
                REQUIRE(sub.lower.z() ==  0);
                REQUIRE(sub.upper.z() ==  4);
            }
            else
            {
                REQUIRE(sub.lower.z() == -4);
                REQUIRE(sub.upper.z() ==  0);
            }
        }
    }
}

TEST_CASE("Region<2>::parent")
{
    Region<2> r({0, 0}, {1, 1});

    auto a = r.parent(0);
    REQUIRE(a.lower.matrix() == Eigen::Vector2d(0, 0));
    REQUIRE(a.upper.matrix() == Eigen::Vector2d(2, 2));

    auto b = r.parent(1);
    REQUIRE(b.lower.matrix() == Eigen::Vector2d(-1, 0));
    REQUIRE(b.upper.matrix() == Eigen::Vector2d(1, 2));

    auto c = r.parent(2);
    REQUIRE(c.lower.matrix() == Eigen::Vector2d(0, -1));
    REQUIRE(c.upper.matrix() == Eigen::Vector2d(2, 1));

    auto d = r.parent(3);
    REQUIRE(d.lower.matrix() == Eigen::Vector2d(-1, -1));
    REQUIRE(d.upper.matrix() == Eigen::Vector2d(1, 1));
}

TEST_CASE("Region<3>::subspace")
{
    Region<3> r({-1, -2, -4}, {1, 2, 4});

    auto a = r.subspace<0>();
    REQUIRE(a.lower.size() == 0);
    REQUIRE(a.upper.size() == 0);

    auto b = r.subspace<1>();
    REQUIRE(b.lower.matrix() == Eigen::Matrix<double, 1, 1>(-1));
    REQUIRE(b.upper.matrix() == Eigen::Matrix<double, 1, 1>(1));

    auto c = r.subspace<3>();
    REQUIRE(c.lower.matrix() == Eigen::Matrix<double, 2, 1>(-1, -2));
    REQUIRE(c.upper.matrix() == Eigen::Matrix<double, 2, 1>(1, 2));

    auto d = r.subspace<5>();
    REQUIRE(d.lower.matrix() == Eigen::Matrix<double, 2, 1>(-1, -4));
    REQUIRE(d.upper.matrix() == Eigen::Matrix<double, 2, 1>(1, 4));
}

TEST_CASE("Region<3>::shrink")
{
    Region<3> r({-1, -2, -4}, {3, 4, 5});
    auto s = r.shrink(0.7);
    REQUIRE(s.lower.x() == Approx(-0.4));
    REQUIRE(s.upper.x() == Approx(2.4));

    REQUIRE(s.lower.y() == Approx(-1.1));
    REQUIRE(s.upper.y() == Approx(3.1));

    REQUIRE(s.lower.z() == Approx(-2.65));
    REQUIRE(s.upper.z() == Approx(3.65));
}

TEST_CASE("Region<3>::withResolution")
{
    Region<3> r({-1, -1, -1}, {1, 1, 1});
    REQUIRE(r.withResolution(5).level == 0);
    REQUIRE(r.withResolution(1).level == 1);
    REQUIRE(r.withResolution(1.2).level == 1);
    REQUIRE(r.withResolution(0.9).level == 2);
}

TEST_CASE("Region<2>::intersection")
{
    Region<2> r({0, 0}, {1, 1});

    bool found = false;
    auto o = r.intersection({-1.0, 0.5}, {1, 0}, &found);

    {
        CAPTURE(o);
        REQUIRE(found);
        REQUIRE(o.col(0).matrix() == Eigen::Vector2d(0, 0.5));
        REQUIRE(o.col(1).matrix() == Eigen::Vector2d(1, 0.5));
    }

    {
        auto o = r.intersection({-1.0, 0.5}, {1, 0.5}, &found);
        REQUIRE(found);
        REQUIRE(o.col(0).matrix() == Eigen::Vector2d(0, 1));
        REQUIRE(o.col(1).matrix() == Eigen::Vector2d(0, 1));
    }

    {
        auto o = r.intersection({-1.0, 0.5}, {0.5, 0.5}, &found);
        REQUIRE(!found);
    }
}
