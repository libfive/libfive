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

TEST_CASE("QEF::solve")
{
    SECTION("Underconstrained (flat surface)")
    {
        QEF<2> q;
        q.insert({1, 0}, {0, 1}, 0);
        q.insert({2, 0}, {0, 1}, 0);

        // This is a flat sheet, without any sharp features in the distance
        // field, so we should end up with the vertex at whatever position
        // we ask to minimize towards.
        {
            auto sol = q.solve();
            CAPTURE(sol.position);
            CAPTURE(sol.value);
            REQUIRE(sol.position == Eigen::Vector2d(0, 0));
            REQUIRE(sol.error == Approx(0.0));
        }

        {
            auto sol = q.solve(Eigen::Vector2d(1, 0), 0);
            CAPTURE(sol.position);
            CAPTURE(sol.value);
            REQUIRE(sol.position == Eigen::Vector2d(1, 0));
            REQUIRE(sol.error == Approx(0.0));
        }

        {   // Here, we expect the solver to walk up on the Y
            // axis by a small amount to meet the desire to
            // drive the value term to 1
            auto sol = q.solve(Eigen::Vector2d(1, 0), 1);
            CAPTURE(sol.position);
            CAPTURE(sol.value);
            REQUIRE(sol.position(0) == Approx(1));
            REQUIRE(sol.position(1) == Approx(0.5));
            REQUIRE(sol.error == Approx(0.0));
            REQUIRE(sol.value == Approx(0.5));
        }
    }

    SECTION("Fully constrained (1D line)")
    {
        QEF<1> q;
        q.insert(Eigen::Matrix<double, 1, 1>(1.0),
                 Eigen::Matrix<double, 1, 1>(1.0),
                 3.0);
        q.insert(Eigen::Matrix<double, 1, 1>(4.0),
                 Eigen::Matrix<double, 1, 1>(-0.5),
                 3.0);

        {
            auto sol = q.solve();
            CAPTURE(sol.position);
            CAPTURE(sol.value);
            REQUIRE(sol.position(0) == Approx(2));
            REQUIRE(sol.error == Approx(0.0));
            REQUIRE(sol.value == Approx(4));
        }

        {
            auto sol = q.solve(Eigen::Matrix<double, 1, 1>(-97.0),
                               103.0);
            CAPTURE(sol.position);
            CAPTURE(sol.value);
            REQUIRE(sol.position(0) == Approx(2));
            REQUIRE(sol.error == Approx(0.0));
            REQUIRE(sol.value == Approx(4));
        }
    }
}

TEST_CASE("QEF::sub")
{
    SECTION("Underconstrained (flat surface)")
    {
        QEF<2> q;
        q.insert({1, 0}, {0, 1}, 0);
        q.insert({2, 0}, {0, 1}, 0);

        // Keep all axes
        QEF<2> q_full = q.sub<3>();

        auto sol_full = q_full.solve(Eigen::Vector2d(1, 0), 0);
        CAPTURE(sol_full.position);
        CAPTURE(sol_full.value);
        REQUIRE(sol_full.position == Eigen::Vector2d(1, 0));
        REQUIRE(sol_full.error == Approx(0.0));

        // Keep the X axis
        QEF<1> q_x = q.sub<1>();

        auto sol_x = q_x.solve(Eigen::Matrix<double, 1, 1>(10), 0);
        CAPTURE(sol_x.position);
        CAPTURE(sol_x.value);
        REQUIRE(sol_x.position == Eigen::Matrix<double, 1, 1>(10));
        REQUIRE(sol_x.error == Approx(0.0));
    }
}
