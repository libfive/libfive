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

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_feature.hpp"
#include "libfive/eval/deck.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("FeatureEvaluator::isInside")
{
    SECTION("Single plane edge")
    {
        auto t = std::make_shared<Deck>(Tree::X());
        FeatureEvaluator a(t);
        REQUIRE(a.isInside({0, 0, 0}) == true);
        REQUIRE(a.isInside({-1, 0, 0}) == true);
        REQUIRE(a.isInside({1, 0, 0}) == false);
    }

    SECTION("2D plane-to-plane (full)")
    {
        auto t = std::make_shared<Deck>(min(Tree::X(), -Tree::X()));
        FeatureEvaluator b(t);
        REQUIRE(b.isInside({0, 0, 0}) == true);
        REQUIRE(b.isInside({1, 0, 0}) == true);
        REQUIRE(b.isInside({-1, 0, 0}) == true);
    }

    SECTION("2D plane-to-plane (empty)")
    {
        auto t = std::make_shared<Deck>(max(Tree::X(), -Tree::X()));
        FeatureEvaluator c(t);
        REQUIRE(c.isInside({0, 0, 0}) == false);
        REQUIRE(c.isInside({1, 0, 0}) == false);
        REQUIRE(c.isInside({-1, 0, 0}) == false);
    }

    SECTION("2D Corner")
    {
        auto t = std::make_shared<Deck>(
                min(min(Tree::X(), -Tree::X()), min(Tree::Y(), -Tree::Y())));
        FeatureEvaluator d(t);
        REQUIRE(d.isInside({0, 0, 0}) == true);
    }

    SECTION("Cube-sphere intersection")
    {
        auto t = std::make_shared<Deck>(
                min(sphere(0.5, {0, 0, 1}),
                    box({-1, -1, -1}, {1, 1, 1})));
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
        auto t = std::make_shared<Deck>(shape);
        FeatureEvaluator e(t);
        auto fs = e.features_({0.1, 0.1, 0});
        CAPTURE(fs.size());
        REQUIRE(!e.isInside({0, 0, 0}));
    }
}

TEST_CASE("FeatureEvaluator::features")
{
    SECTION("Single feature")
    {
        auto t = std::make_shared<Deck>(Tree::X());
        FeatureEvaluator e(t);
        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 1);
        REQUIRE(fs.front() == Eigen::Vector3f(1, 0, 0));
    }

    SECTION("Two features (min)")
    {
        auto t = std::make_shared<Deck>(min(Tree::X(), -Tree::X()));
        FeatureEvaluator e(t);

        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE(*(i++) == Eigen::Vector3f(1, 0, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(-1, 0, 0));
    }

    SECTION("Two features (max)")
    {
        auto t = std::make_shared<Deck>(max(Tree::X(), -Tree::X()));
        FeatureEvaluator e(t);

        auto fs = e.features({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE(*(i++) == Eigen::Vector3f(-1, 0, 0));
        REQUIRE(*(i++) == Eigen::Vector3f(1, 0, 0));
    }

    SECTION("Three features")
    {
        auto t = std::make_shared<Deck>(
                min(Tree::X(), min(Tree::Y(), Tree::Z())));
        FeatureEvaluator e(t);

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
        auto t = std::make_shared<Deck>(rectangle(-1, 0, -1, 1));
        FeatureEvaluator e(t);

        REQUIRE(e.features({-0.5, -1, 0}).size() == 1);
    }

    SECTION("One feature (nested)")
    {
        auto r = max(max(max(-Tree::X(), Tree::X() - 1),
                         max(-Tree::Y(), Tree::Y() - 1)),
                    -Tree::X());
        auto t = std::make_shared<Deck>(r);
        FeatureEvaluator e(t);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated)")
    {
        auto r = max(Tree::X(), Tree::X());
        auto t = std::make_shared<Deck>(r);
        FeatureEvaluator e(t);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated multiple times)")
    {
        auto r = max(Tree::X(), max(Tree::X(), Tree::X()));
        auto t = std::make_shared<Deck>(r);
        FeatureEvaluator e(t);

        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("One feature (duplicated even more times)")
    {
        auto r = max(max(Tree::X(), Tree::X()), max(Tree::X(), Tree::X()));
        auto t = std::make_shared<Deck>(r);
        FeatureEvaluator e(t);
        REQUIRE(e.features({0, 0.2f, 0}).size() == 1);
    }

    SECTION("Coincident planes with same normal")
    {
        auto r = max(Tree::Z() - 6, Tree::Z() + -6);
        auto t = std::make_shared<Deck>(r);
        FeatureEvaluator e(t);
        REQUIRE(e.features({0, 0, 6}).size() == 1);
    }
}
