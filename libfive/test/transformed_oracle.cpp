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

#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/tree/oracle.hpp"
#include "oracles.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

/*  In order to test the transformed primitives, we can't test that the meshing
 *  is identical when they're used, since numeric error can cause them not to
 *  be.  So instead, the evaluators are tested to ensure they return the same 
 *  results, up to numeric error.
 */

void compareUnderTransformation(Tree oracleTree, Tree controlTree,
    std::function<Tree(Tree)> transformation,
    std::vector<Eigen::Vector3f> testPoints)
{
    auto transformedOracle = transformation(oracleTree);
    auto transformedControl = transformation(controlTree);
    auto oTape = std::make_shared<Tape>(transformedOracle);
    auto cTape = std::make_shared<Tape>(transformedControl);
    {
        DerivArrayEvaluator o(oTape, 0);
        DerivArrayEvaluator c(cTape, 0);
        for (auto i = 0; i < testPoints.size(); ++i)
        {
            o.set(testPoints[i], i);
            c.set(testPoints[i], i);
        }
        auto oResults = o.derivs(testPoints.size());
        auto cResults = c.derivs(testPoints.size());
        for (auto i = 0; i < testPoints.size(); ++i)
        {
            CAPTURE(i);
            CAPTURE(oResults.col(i));
            CAPTURE(cResults.col(i));
            Eigen::Vector4f diff = oResults.col(i) - cResults.col(i);
            auto relativeClose = oResults.col(i).isApprox(cResults.col(i));
            if (!relativeClose) 
            {
                REQUIRE(diff.norm() < 1e-4);
            }
            else
            {
                REQUIRE(true); 
                //To keep the number of require statements constant.
            }
        }
        /*  getAmbiguous is numerically unstable, of course, so while I think
         *  the numerical instability will be of a sort to not affect it, test
         *  failure may be due to being mistaken on that rather than a flaw in
         *  the main code.
         */
        for (auto i = 0; i < testPoints.size(); ++i)
        {
        CAPTURE(i);
        REQUIRE((o.getAmbiguous(i) == c.getAmbiguous(i)).all());
        }
    }
    {
        FeatureEvaluator o(oTape, 0);
        FeatureEvaluator c(cTape, 0);

        for (auto point : testPoints)
        {
            auto oFeatures = o.featuresAt(point);
            auto cFeatures = c.featuresAt(point);
            CAPTURE(point);
            CAPTURE(oFeatures.size());
            CAPTURE(cFeatures.size());
            /*  We would like to capture which feature is different, but the
             *  fact that they may be permuted, plus the fact that several
             *  types of things can be different, makes it more convenient
             *  to add any necessary statements only as needed.
             */

            auto valid = std::is_permutation(oFeatures.begin(), oFeatures.end(), 
                cFeatures.begin(), [](Feature& first, Feature& second)
                {
                    auto firstEpsilons = first.getEpsilons();
                    auto secondEpsilons = second.getEpsilons();
                    return (first.deriv - second.deriv).norm() < 1e-4 &&
                    std::is_permutation(
                        firstEpsilons.begin(), firstEpsilons.end(),
                        secondEpsilons.begin(), 
                        [](Eigen::Vector3d& first, Eigen::Vector3d& second)
                    { return (first - second).norm() < (1e-4); });
                });
            REQUIRE(std::is_permutation(oFeatures.begin(), oFeatures.end(), 
                cFeatures.begin(), [](Feature& first, Feature& second)
                {
                    auto firstEpsilons = first.getEpsilons();
                    auto secondEpsilons = second.getEpsilons();
                    return (first.deriv - second.deriv).norm() < 1e-4 &&
                    std::is_permutation(
                        firstEpsilons.begin(), firstEpsilons.end(),
                        secondEpsilons.begin(), 
                        [](Eigen::Vector3d& first, Eigen::Vector3d& second) 
                            { return (first - second).norm() < (1e-4); });
                }));
        /*  isInside is also numerically unstable, likely in a way that does
         *  preclude testing it.
         */
        for (auto point : testPoints)
        {
            auto oDeriv = o.deriv(point);
            auto cDeriv = c.deriv(point);
            CAPTURE(point);
            CAPTURE(cDeriv);
            CAPTURE(oDeriv);
            REQUIRE(oDeriv.isApprox(cDeriv) ||
                (oDeriv - cDeriv).norm() < 1e-4);
        }
        /*  IntervalEvaluator cannot be comparison-tested; while both
         *  approaches should return results that contain the actual
         *  interval, neither is required to be exact.
         */
    }
}

TEST_CASE("Rotated Oracle: Render and compare (cube)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle = convertToOracleAxes(cube);
    compareUnderTransformation(cubeOracle, cube, 
        [](Tree t) {return rotate2d(t, 10);},
        { {1.5, 1.5, 1.5},{ -1.5, -1.5, 1.5 },
        { -1.5, 1.5, -1.5 },{ 1.5, -1.5, -1.5 }, {0., 0., 0.} });
    /*  Numeric error means that with a rotation we can't really hit where
     *  interesting stuff is going to happen anyway, except for 0.
     */
}

TEST_CASE("Rotated Oracle: Render and compare (cube as oracle)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle(std::make_unique<cubeAsOracle>());

    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {return rotate2d(t, 10);},
        { { 1.5, 1.5, 1.5 },{ -1.5, -1.5, 1.5 },
        { -1.5, 1.5, -1.5 },{ 1.5, -1.5, -1.5 },
        { 0., 0., 0. } });
}

TEST_CASE("Abs and skew applied to Oracle: "
    "Render and compare (cube)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle = convertToOracleAxes(cube);
    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {
        return t.remap(Tree::Y(), Tree::X(), 
            abs(Tree::Z() + Tree::X() * 0.2f));},
        { { 1.5, 1.5, 1.8 },{ -1.5, -1.5, 1.8},
        { -1.5, 1.5, -1.8 },{ 1.5, -1.5, -1.8 }, { 0., 0., 0. } });
        //  Some of these points are maps of corners, some are not.

}

TEST_CASE("Abs and skew applied to Oracle: "
    "Render and compare (cube as oracle)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle(std::make_unique<cubeAsOracle>());
    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {
        return t.remap(Tree::Y(), Tree::X(),
            abs(Tree::Z() + Tree::X() * 0.2f));},
        { { 1.5, 1.5, 1.8 },{ -1.5, -1.5, 1.8 },
        { -1.5, 1.5, -1.8 },{ 1.5, -1.5, -1.8 },{ 0., 0., 0. } });
    //  Some of these points correspond to corners, some do not.
}

TEST_CASE("Jacobian-0 transform and abs applied to Oracle: "
    "Render and compare (cube)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle = convertToOracleAxes(cube);

    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {
        return t.remap(Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
            abs(Tree::Z()) * Tree::X());},
        { { 1.5, 1., 1. },{ -1., -1.5, 1. },
        { -1., 1.5, -1. },{ 1., -1., -1. },{ 0., 0., 0. } });
    //  None of these correspond to corners

   cube = max(max(
        max(-(Tree::X() + 1.),
            Tree::X() - 1.),
        max(-(Tree::Y() + 1.),
            Tree::Y() - 1.)),
        max(-(Tree::Z() + 1.),
            Tree::Z() - 1.));
    cubeOracle = convertToOracleAxes(cube);

    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {
        return t.remap(Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
            abs(Tree::Z()) * Tree::X());},
        { { 1., 1., 1. },{ -1., -1., 1. },
        { -1., 1., -1. },{ 1., -1., -1. },{ 0., 0., 0. } });
    //  But these do
}

TEST_CASE("Jacobian-0 transform and abs applied to Oracle: "
    "Render and compare (cube as oracle)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Tree cubeOracle(std::make_unique<cubeAsOracle>());

    compareUnderTransformation(cubeOracle, cube,
        [](Tree t) {
        return t.remap(Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
            abs(Tree::Z()) * Tree::X());},
        { { 1.5, 1., 1. },{ -1., -1.5, 1. },
        { -1., 1.5, -1. },{ 1., -1., -1. },{ 0., 0., 0. } });
    //  None of these correspond to corners
}