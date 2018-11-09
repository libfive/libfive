/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/region.hpp"

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/oracle/oracle_clause.hpp"

#include "util/shapes.hpp"
#include "util/oracles.hpp"

using namespace Kernel;

// Compares two BRep objects using Catch macros
template <unsigned N>
void BRepCompare(const BRep<N>& first, const BRep<N>& second)
{
    REQUIRE(first.verts.size() == second.verts.size());
    for (unsigned i = 0; i < first.verts.size(); ++i) {
        CAPTURE(i);
        CAPTURE(first.verts[i]);
        CAPTURE(second.verts[i]);
        REQUIRE((first.verts[i] - second.verts[i]).norm() < 1e-6);
    }

    REQUIRE(first.branes.size() == second.branes.size());
    for (unsigned i = 0; i < first.branes.size(); ++i) {
        CAPTURE(i);
        CAPTURE(first.branes[i]);
        CAPTURE(second.branes[i]);
        REQUIRE(first.branes[i] == second.branes[i]);
    }
}

/*  In order to test the primitives system, we take some basic shapes and 
 *  ensure that the meshing is completely unchanged when X, Y, and Z are
 *  replaced by their oracle equivalents.
 */
TEST_CASE("Oracle: render and compare (sphere)")
{
  Tree s = sphere(0.5);
  Region<3> r({ -1, -1, -1 }, { 1, 1, 1 });
  Tree sOracle = convertToOracleAxes(s);

  // We can't use multithreading, because it causes triangles to be
  // output in a non-deterministic order, which fails the comparison.
  auto mesh = Mesh::render(sOracle, r, 0.1, 1e-8, false);
  auto comparisonMesh = Mesh::render(s, r, 0.1, 1e-8, false);

  BRepCompare(*mesh, *comparisonMesh);
}

TEST_CASE("Oracle: render and compare (cube)")
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

  auto mesh = Mesh::render(cubeOracle, r, 0.1, 1e-8, false);
  auto comparisonMesh = Mesh::render(cube, r, 0.1, 1e-8, false);

  BRepCompare(*mesh, *comparisonMesh);
}

TEST_CASE("Oracle: render and compare (cube as oracle)")
{
    auto cube = max(max(
        max(-(Tree::X() + 1.5),
            Tree::X() - 1.5),
        max(-(Tree::Y() + 1.5),
            Tree::Y() - 1.5)),
        max(-(Tree::Z() + 1.5),
            Tree::Z() - 1.5));

    Tree cubeOracle(std::unique_ptr<CubeOracleClause>(new CubeOracleClause()));

    //  The region is set so we hit where the interesting stuff happens.
    Region<3> r({ -3., -3., -3. }, { 3., 3., 3. });

    auto mesh = Mesh::render(cubeOracle, r, 1.6);
    auto comparisonMesh = Mesh::render(cube, r);

    BRepCompare(*mesh, *comparisonMesh);
}
