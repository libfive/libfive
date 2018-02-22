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

TEST_CASE("Oracle: Render and compare (sphere)")
{
  Tree s = sphere(0.5);
  Region<3> r({ -1, -1, -1 }, { 1, 1, 1 });
  Tree sOracle = convertToOracleAxes(s);

  auto mesh = Mesh::render(sOracle, r);
  auto comparisonMesh = Mesh::render(s, r);

  requireEquality(*mesh, *comparisonMesh);
}

TEST_CASE("Oracle: Render and compare (cube)")
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

  auto mesh = Mesh::render(cubeOracle, r);
  auto comparisonMesh = Mesh::render(cube, r);

  requireEquality(*mesh, *comparisonMesh);
}

TEST_CASE("Oracle: Render and compare (cube as oracle)")
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

    auto mesh = Mesh::render(cubeOracle, r);
    auto comparisonMesh = Mesh::render(cube, r);

    requireEquality(*mesh, *comparisonMesh);
}