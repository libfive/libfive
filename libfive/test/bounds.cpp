/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/tree/tree.hpp"

#define ENABLE_FIND_BOUNDS_EXPERIMENTAL
#include "libfive/solve/bounds.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("findBounds")
{
    SECTION("Simple sphere")
    {
        auto s = sphere(0.5);
        auto r = findBounds(s);
        CAPTURE(r.lower);
        CAPTURE(r.upper);
        REQUIRE(r.lower.x() == Approx(-0.5).margin(0.001));
        REQUIRE(r.lower.y() == Approx(-0.5).margin(0.001));
        REQUIRE(r.lower.z() == Approx(-0.5).margin(0.001));

        REQUIRE(r.upper.x() == Approx(0.5).margin(0.001));
        REQUIRE(r.upper.y() == Approx(0.5).margin(0.001));
        REQUIRE(r.upper.z() == Approx(0.5).margin(0.001));
    }

    SECTION("Moved sphere")
    {
        auto s = move(sphere(0.5), {3, 4, 5});
        auto r = findBounds(s);
        CAPTURE(r.lower);
        CAPTURE(r.upper);
        REQUIRE(r.lower.x() == Approx(2.5).margin(0.001));
        REQUIRE(r.lower.y() == Approx(3.5).margin(0.001));
        REQUIRE(r.lower.z() == Approx(4.5).margin(0.001));

        REQUIRE(r.upper.x() == Approx(3.5).margin(0.001));
        REQUIRE(r.upper.y() == Approx(4.5).margin(0.001));
        REQUIRE(r.upper.z() == Approx(5.5).margin(0.001));
    }

    SECTION("Moved circle")
    {
        auto s = move(circle(0.3), {0.3f, 0.3f, 0.f});

        auto r = findBounds(s);
        CAPTURE(r.lower);
        CAPTURE(r.upper);
        REQUIRE(r.lower.x() == Approx(0).margin(0.001));
        REQUIRE(r.lower.y() == Approx(0).margin(0.001));

        REQUIRE(r.upper.x() == Approx(0.6).margin(0.001));
        REQUIRE(r.upper.y() == Approx(0.6).margin(0.001));
    }

    SECTION("Rotated shape (2D)")
    {
        auto s = rotate2d(circle(0.5), M_PI/4);
        auto r = findBounds(s);

        CAPTURE(r.lower);
        CAPTURE(r.upper);

        REQUIRE(!std::isinf(r.lower.x()));
        REQUIRE(!std::isinf(r.lower.y()));
        REQUIRE(r.lower.x() <= -0.5);
        REQUIRE(r.lower.y() <= -0.5);
        REQUIRE(r.lower.x() > -1);
        REQUIRE(r.lower.y() > -1);
        REQUIRE(std::isinf(r.lower.z()));

        REQUIRE(!std::isinf(r.upper.x()));
        REQUIRE(!std::isinf(r.upper.y()));
        REQUIRE(r.upper.x() >= 0.5);
        REQUIRE(r.upper.y() >= 0.5);
        REQUIRE(r.lower.x() < 1);
        REQUIRE(r.lower.y() < 1);
        REQUIRE(std::isinf(r.upper.z()));
    }

    SECTION("Rotated shape (3D)")
    {
        auto s = rotate2d(sphere(0.5), M_PI/4);
        auto r = findBounds(s);

        CAPTURE(r.lower);
        CAPTURE(r.upper);

        REQUIRE(!std::isinf(r.lower.x()));
        REQUIRE(!std::isinf(r.lower.y()));
        REQUIRE(!std::isinf(r.lower.z()));
        REQUIRE(r.lower.x() <= -0.5);
        REQUIRE(r.lower.y() <= -0.5);
        REQUIRE(r.lower.z() <= -0.5);
        REQUIRE(r.lower.x() > -0.55);
        REQUIRE(r.lower.y() > -0.55);
        REQUIRE(r.lower.z() > -0.501);

        REQUIRE(!std::isinf(r.upper.x()));
        REQUIRE(!std::isinf(r.upper.y()));
        REQUIRE(!std::isinf(r.upper.z()));
        REQUIRE(r.upper.x() >= 0.5);
        REQUIRE(r.upper.y() >= 0.5);
        REQUIRE(r.upper.z() >= 0.5);
        REQUIRE(r.lower.x() < 0.55);
        REQUIRE(r.lower.y() < 0.55);
        REQUIRE(r.lower.z() < 0.501);
    }

    SECTION("Reflected shape")
    {
        auto s = circle(0.5).remap(Tree::Y(), Tree::X(), Tree::Z());
        auto r = findBounds(s);

        CAPTURE(r.lower);
        CAPTURE(r.upper);

        REQUIRE(!std::isinf(r.lower.x()));
        REQUIRE(!std::isinf(r.lower.y()));
        REQUIRE(r.lower.x() <= -0.5);
        REQUIRE(r.lower.y() <= -0.5);
        REQUIRE(r.lower.x() > -1);
        REQUIRE(r.lower.y() > -1);
        REQUIRE(std::isinf(r.lower.z()));

        REQUIRE(!std::isinf(r.upper.x()));
        REQUIRE(!std::isinf(r.upper.y()));
        REQUIRE(r.upper.x() >= 0.5);
        REQUIRE(r.upper.y() >= 0.5);
        REQUIRE(r.lower.x() < 1);
        REQUIRE(r.lower.y() < 1);
        REQUIRE(std::isinf(r.upper.z()));
    }

    SECTION("Tiny transform with numerical implications")
    {
        // Rotate by very close to 90 degrees
        auto s = circle(0.5).remap(Tree::X() - Tree::Y()*0.0001,
                                   Tree::Y() + Tree::X()*0.0001, Tree::Z());
        auto r = findBounds(s);

        CAPTURE(r.lower);
        CAPTURE(r.upper);

        REQUIRE(!std::isinf(r.lower.x()));
        REQUIRE(!std::isinf(r.lower.y()));
        REQUIRE(r.lower.x() <= -0.5);
        REQUIRE(r.lower.y() <= -0.5);
        REQUIRE(r.lower.x() > -1);
        REQUIRE(r.lower.y() > -1);
        REQUIRE(std::isinf(r.lower.z()));

        REQUIRE(!std::isinf(r.upper.x()));
        REQUIRE(!std::isinf(r.upper.y()));
        REQUIRE(r.upper.x() >= 0.5);
        REQUIRE(r.upper.y() >= 0.5);
        REQUIRE(r.lower.x() < 1);
        REQUIRE(r.lower.y() < 1);
        REQUIRE(std::isinf(r.upper.z()));
    }
}
