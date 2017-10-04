#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_feature.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("FeatureEvaluator::isInside")
{
    SECTION("Single plane edge")
    {
        Tape t(Tree::X());
        FeatureEvaluator a(t);
        REQUIRE(a.isInside({0, 0, 0}) == true);
        REQUIRE(a.isInside({-1, 0, 0}) == true);
        REQUIRE(a.isInside({1, 0, 0}) == false);
    }

    SECTION("2D plane-to-plane (full)")
    {
        Tape t(min(Tree::X(), -Tree::X()));
        FeatureEvaluator b(t);
        REQUIRE(b.isInside({0, 0, 0}) == true);
        REQUIRE(b.isInside({1, 0, 0}) == true);
        REQUIRE(b.isInside({-1, 0, 0}) == true);
    }

    SECTION("2D plane-to-plane (empty)")
    {
        Tape t(max(Tree::X(), -Tree::X()));
        FeatureEvaluator c(t);
        REQUIRE(c.isInside({0, 0, 0}) == false);
        REQUIRE(c.isInside({1, 0, 0}) == false);
        REQUIRE(c.isInside({-1, 0, 0}) == false);
    }

    SECTION("2D Corner")
    {
        Tape t(min(min(Tree::X(), -Tree::X()), min(Tree::Y(), -Tree::Y())));
        FeatureEvaluator d(t);
        REQUIRE(d.isInside({0, 0, 0}) == true);
    }
}

TEST_CASE("FeatureEvaluator::isAmbiguous")
{
    SECTION("Single feature")
    {
        Tape t(Tree::X());
        FeatureEvaluator e(t);
        REQUIRE(e.isAmbiguous({0, 0, 0}) == false);
        REQUIRE(e.isAmbiguous({1, 0, 0}) == false);
    }

    SECTION("Two features (min)")
    {
        Tape t(min(Tree::X(), -Tree::X()));
        FeatureEvaluator e(t);
        REQUIRE(e.isAmbiguous({0, 0, 0}) == true);
        REQUIRE(e.isAmbiguous({1, 0, 0}) == false);
    }
}

TEST_CASE("FeatureEvaluator::featuresAt")
{
    SECTION("Single feature")
    {
        Tape t(Tree::X());
        FeatureEvaluator e(t);
        auto fs = e.featuresAt({0, 0, 0});
        REQUIRE(fs.size() == 1);
        REQUIRE(fs.front().deriv == Eigen::Vector3d(1, 0, 0));
    }

    SECTION("Two features (min)")
    {
        Tape t(min(Tree::X(), -Tree::X()));
        FeatureEvaluator e(t);

        auto fs = e.featuresAt({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE((i++)->deriv == Eigen::Vector3d(1, 0, 0));
        REQUIRE((i++)->deriv == Eigen::Vector3d(-1, 0, 0));
    }

    SECTION("Two features (max)")
    {
        Tape t(max(Tree::X(), -Tree::X()));
        FeatureEvaluator e(t);

        auto fs = e.featuresAt({0, 0, 0});
        REQUIRE(fs.size() == 2);
        auto i = fs.begin();
        REQUIRE((i++)->deriv == Eigen::Vector3d(1, 0, 0));
        REQUIRE((i++)->deriv == Eigen::Vector3d(-1, 0, 0));
    }

    SECTION("Three features")
    {
        Tape t(min(Tree::X(), min(Tree::Y(), Tree::Z())));
        FeatureEvaluator e(t);

        auto fs = e.featuresAt({0, 0, 0});
        REQUIRE(fs.size() == 3);

        auto i = fs.begin();
        REQUIRE((i++)->deriv == Eigen::Vector3d(1, 0, 0));
        REQUIRE((i++)->deriv == Eigen::Vector3d(0, 1, 0));
        REQUIRE((i++)->deriv == Eigen::Vector3d(0, 0, 1));
    }

    SECTION("Buried ambiguity")
    {
        // The ambiguity here (in max(-1 - X, X) is irrelevant, as
        // it ends up being masked by the Y clause)
        Tape t(rectangle(-1, 0, -1, 1));
        FeatureEvaluator e(t);

        REQUIRE(e.featuresAt({-0.5, -1, 0}).size() == 1);
    }

    SECTION("One feature (nested)")
    {
        auto r = max(max(max(-Tree::X(), Tree::X() - 1),
                         max(-Tree::Y(), Tree::Y() - 1)),
                    -Tree::X());
        Tape t(r);
        FeatureEvaluator e(t);

        REQUIRE(e.featuresAt({0, 0.2, 0}).size() == 1);
    }

    SECTION("One feature (duplicated)")
    {
        auto r = max(Tree::X(), Tree::X());
        Tape t(r);
        FeatureEvaluator e(t);

        REQUIRE(e.featuresAt({0, 0.2, 0}).size() == 1);
    }

    SECTION("One feature (duplicated multiple times)")
    {
        auto r = max(Tree::X(), max(Tree::X(), Tree::X()));
        Tape t(r);
        FeatureEvaluator e(t);

        REQUIRE(e.featuresAt({0, 0.2, 0}).size() == 1);
    }

    SECTION("One feature (duplicated even more times)")
    {
        auto r = max(max(Tree::X(), Tree::X()), max(Tree::X(), Tree::X()));
        Tape t(r);
        FeatureEvaluator e(t);
        REQUIRE(e.featuresAt({0, 0.2, 0}).size() == 1);
    }

    SECTION("Coincident planes with same normal")
    {
        auto r = max(Tree::Z() - 6, Tree::Z() + -6);
        Tape t(r);
        FeatureEvaluator e(t);
        REQUIRE(e.featuresAt({0, 0, 6}).size() == 1);
    }
}

TEST_CASE("FeatureEvaluator::getAmbiguous")
{
    Tape t(min(Tree::X(), -Tree::X()));
    FeatureEvaluator e(t);
    e.set({0, 0, 0}, 0);
    e.set({1, 0, 0}, 1);
    e.set({2, 0, 0}, 2);
    e.set({0, 0, 0}, 3);

    e.values(4);

    auto a = e.getAmbiguous(3);
    REQUIRE(a.count() == 1);
    REQUIRE(a(0) == 1);

    auto b = e.getAmbiguous(4);
    REQUIRE(b.count() == 2);
    REQUIRE(b(0) == 1);
    REQUIRE(b(3) == 1);
}

TEST_CASE("FeatureEvaluator::push(Feature)")
{
    Tape t(min(Tree::X(), -Tree::X()));
    FeatureEvaluator e(t);
    REQUIRE(e.eval({0, 0, 0}) == 0); // Force an ambiguous evaluation
    Feature f;

    SECTION("LHS")
    {   // Use a dummy feature to select the first branch
        REQUIRE(f.push({1, 0, 0}, {1, 0}));
        e.push(f);
        REQUIRE(e.eval({1, 0, 0}) == 1);
        REQUIRE(t.utilization() < 1);
    }

    SECTION("RHS")
    {   // Use a dummy feature to select the second branch
        REQUIRE(f.push({-1, 0, 0}, {1, 1}));
        e.push(f);
        REQUIRE(e.eval({-2, 0, 0}) == 2);
        REQUIRE(t.utilization() < 1);
    }
}
