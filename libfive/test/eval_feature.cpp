/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_feature.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("FeatureEvaluator::isInside")
{
    SECTION("Single plane edge")
    {
        FeatureEvaluator a(Tree::X());
        REQUIRE(a.isInside({0, 0, 0}) == true);
        REQUIRE(a.isInside({-1, 0, 0}) == true);
        REQUIRE(a.isInside({1, 0, 0}) == false);
    }

    SECTION("2D plane-to-plane (full)")
    {
        FeatureEvaluator b(min(Tree::X(), -Tree::X()));
        REQUIRE(b.isInside({0, 0, 0}) == true);
        REQUIRE(b.isInside({1, 0, 0}) == true);
        REQUIRE(b.isInside({-1, 0, 0}) == true);
    }

    SECTION("2D plane-to-plane (empty)")
    {
        FeatureEvaluator c(max(Tree::X(), -Tree::X()));
        REQUIRE(c.isInside({0, 0, 0}) == false);
        REQUIRE(c.isInside({1, 0, 0}) == false);
        REQUIRE(c.isInside({-1, 0, 0}) == false);
    }

    SECTION("2D Corner")
    {
        auto t = min(min(Tree::X(), -Tree::X()), min(Tree::Y(), -Tree::Y()));
        FeatureEvaluator d(t);
        REQUIRE(d.isInside({0, 0, 0}) == true);
    }

    SECTION("Cube-sphere intersection")
    {
        auto t = min(sphere(0.5, {0, 0, 1}),
                     box({-1, -1, -1}, {1, 1, 1}));
        FeatureEvaluator d(t);
        REQUIRE(d.isInside({0, 0, 0}) == true);
        REQUIRE(d.isInside({0.5, 0, 1}) == true);
        REQUIRE(d.isInside({-0.5, 0, 1}) == true);
        REQUIRE(d.isInside({0, 0.5, 1}) == true);
        REQUIRE(d.isInside({0, -0.5, 1}) == true);
        REQUIRE(d.isInside({0, 0, 1.5}) == true);

        REQUIRE(d.isInside({0, 0, 2}) == false);
    }

    SECTION("Hollow cylinder")
    {
        auto shape = max(box({-50, -50, 0}, {50, 50, 10}),
            max(cylinder(10, 100), -cylinder(5, 100)));
        FeatureEvaluator e(shape);
        auto fs = e.features_({0.1f, 0.1f, 0.0f});
        CAPTURE(fs.size());
        REQUIRE(!e.isInside({0.0f, 0.0f, 0.0f}));
    }

    SECTION("Box-box intersection")
    {
        auto b = max(box({-1, 0, -1}, {1, 2, -0.2}),
                    -box({0.1, 0, -1}, {1, 2, -0.1}));
        FeatureEvaluator e(b);
        REQUIRE(!e.isInside({0.625f, 0.0f, -1.0f}));
    }

    SECTION("Other box-box intersection")
    {
        auto b = max(max(Tree::X(), Tree::Y()),
                    -max(Tree::X(), Tree::Y()));
        FeatureEvaluator e(b);

        auto fs = e.features_({0.0, 0.0f, -0.5f});
        REQUIRE(!e.isInside({0.0, 0.0f, -0.5f}));
    }
}

TEST_CASE("FeatureEvaluator::features")
{
    SECTION("Single feature")
    {
        FeatureEvaluator e(Tree::X());
        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 1);
        REQUIRE(fs.front() == Eigen::Vector3f(1, 0, 0));
    }

    SECTION("Two features (min)")
    {
        FeatureEvaluator e(min(Tree::X(), -Tree::X()));

        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE(*(i++) == Eigen::Vector3f(1, 0, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(-1, 0, 0));
    }

    SECTION("Two features (max)")
    {
        FeatureEvaluator e(max(Tree::X(), -Tree::X()));

        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE(*(i++) == Eigen::Vector3f(-1, 0, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(1, 0, 0));
    }

    SECTION("Three features")
    {
        FeatureEvaluator e(min(Tree::X(), min(Tree::Y(), Tree::Z())));

        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 3);

        auto i = fs.begin();
        REQUIRE(*(i++) == Eigen::Vector3f(1, 0, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(0, 1, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(0, 0, 1));
    }

    SECTION("Buried ambiguity")
    {
        // The ambiguity here (in max(-1 - X, X) is irrelevant, as
        // it ends up being masked by the Y clause)
        FeatureEvaluator e(rectangle(-1, 0, -1, 1));

        REQUIRE(e.features({-0.5, -1, 0}).size() == 1);
    }

    SECTION("One feature (nested)")
    {
        auto r = max(max(max(-Tree::X(), Tree::X() - 1),
                         max(-Tree::Y(), Tree::Y() - 1)),
                    -Tree::X());
        FeatureEvaluator e(r);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated)")
    {
        auto r = max(Tree::X(), Tree::X());
        FeatureEvaluator e(r);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated multiple times)")
    {
        auto r = max(Tree::X(), max(Tree::X(), Tree::X()));
        FeatureEvaluator e(r);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated even more times)")
    {
        auto r = max(max(Tree::X(), Tree::X()), max(Tree::X(), Tree::X()));
        FeatureEvaluator e(r);
        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("Coincident planes with same normal")
    {
        auto r = max(Tree::Z() - 6, Tree::Z() + -6);
        FeatureEvaluator e(r);
        REQUIRE(e.features({0, 0, 6}).size() == 1);
    }

    SECTION("Feature deduplication")
    {
        auto r = max(Tree::X(), Tree::X() + Tree::Y() * 1e-8);
        FeatureEvaluator e(r);
        REQUIRE(e.features({0, 0, 0}).size() == 1);
    }
}
