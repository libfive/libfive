/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/simplex/qef.hpp"

using namespace Kernel;

TEST_CASE("QEF<2>::solve", "[!mayfail]")
{
    SECTION("Underconstrained (flat surface)")
    {
        QEF<2> q;
        q.insert({0, 1}, {0, 0}, 0);
        q.insert({0, 1}, {1, 0}, 0);

        // This is a flat sheet, without any sharp features in the distance
        // field, so we should end up with the vertex at whatever position
        // we ask to minimize towards.
        REQUIRE(q.solve().position == Eigen::Matrix<double, 2, 1>::Zero());
        REQUIRE(q.solve({1, 0}).position == Eigen::Matrix<double, 2, 1>(1, 0));
    }
}
