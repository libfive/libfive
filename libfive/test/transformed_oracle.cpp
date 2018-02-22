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

/*  In order to test the primitives system, we take some basic shapes and
*  ensure that the meshing is completely unchanged when X, Y, and Z are
*  replaced by their oracle equivalents.
*/

TEST_CASE("Rotated Oracle: Render and compare (cube)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));
    Region<3> r({ -2.5, -2.5, -2.5 }, { 2.5, 2.5, 2.5 });
    Tree cubeOracle = convertToOracleAxes(cube);

    auto mesh = Mesh::render(rotate2d(cubeOracle, 10), r);
    auto comparisonMesh = Mesh::render(rotate2d(cube, 10), r);

    requireEquality(*mesh, *comparisonMesh);
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
    Region<3> r({ -3., -3., -3. }, { 3., 3., 3. });
    //The region is set so we hit where the interesting stuff happens.
    Tree cubeOracle(std::make_unique<cubeAsOracle>());

    auto mesh = Mesh::render(rotate2d(cubeOracle, 10), r);
    auto comparisonMesh = Mesh::render(rotate2d(cube, 10), r);

    requireEquality(*mesh, *comparisonMesh);
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
    Region<3> r({ -2.5, -2.5, -2.5 }, { 2.5, 2.5, 2.5 });
    Tree cubeOracle = convertToOracleAxes(cube);

    auto mesh = Mesh::render(cubeOracle.remap(
        Tree::Y(), Tree::X(), abs(Tree::Z() + Tree::X() * 0.2f)), r);
    auto comparisonMesh = Mesh::render(cube.remap(
        Tree::Y(), Tree::X(), abs(Tree::Z() + Tree::X() * 0.2f)), r);

    requireEquality(*mesh, *comparisonMesh);
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
    Region<3> r({ -3., -3., -3. }, { 3., 3., 3. });
    //The region is set so we hit where the interesting stuff happens.
    Tree cubeOracle(std::make_unique<cubeAsOracle>());

    auto mesh = Mesh::render(cubeOracle.remap(
        Tree::Y(), Tree::X(), abs(Tree::Z() + Tree::X() * 0.2f)), r);
    auto comparisonMesh = Mesh::render(cube.remap(
        Tree::Y(), Tree::X(), abs(Tree::Z() + Tree::X() * 0.2f)), r);

    requireEquality(*mesh, *comparisonMesh);
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
    Region<3> r({ -2.5, -2.5, -2.5 }, { 2.5, 2.5, 2.5 });
    Tree cubeOracle = convertToOracleAxes(cube);

    auto mesh = Mesh::render(cubeOracle.remap(
        Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()), 
        abs(Tree::Z()) * Tree::X()), r);
    auto comparisonMesh = Mesh::render(cube.remap(
        Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
        abs(Tree::Z()) * Tree::X()), r);

    requireEquality(*mesh, *comparisonMesh);
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
    Region<3> r({ -3., -3., -3. }, { 3., 3., 3. });
    //The region is set so we hit where the interesting stuff happens.
    Tree cubeOracle(std::make_unique<cubeAsOracle>());

    auto mesh = Mesh::render(cubeOracle.remap(
        Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
        abs(Tree::Z()) * Tree::X()), r);
    auto comparisonMesh = Mesh::render(cube.remap(
        Tree::X() * Tree::Y(), Tree::Y() * abs(Tree::Z()),
        abs(Tree::Z()) * Tree::X()), r);

    requireEquality(*mesh, *comparisonMesh);
}