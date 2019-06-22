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
#include "libfive/render/brep/settings.hpp"

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/oracle/oracle_clause.hpp"

#include "libfive/eval/eval_deriv_array.hpp"

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
  BRepSettings settings;
  settings.workers = 1;
  settings.min_feature = 0.1;
  auto mesh = Mesh::render(sOracle, r, settings);
  auto comparisonMesh = Mesh::render(s, r, settings);

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

  BRepSettings settings;
  settings.workers = 1;
  settings.min_feature = 0.1;

  auto mesh = Mesh::render(cubeOracle, r, settings);
  auto comparisonMesh = Mesh::render(cube, r, settings);

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

    BRepSettings settings;
    settings.workers = 1;
    settings.min_feature = 1.6;

    auto mesh = Mesh::render(cubeOracle, r, settings);
    auto comparisonMesh = Mesh::render(cube, r, settings);

    BRepCompare(*mesh, *comparisonMesh);
}

////////////////////////////////////////////////////////////////////////////////

class PickySIMDOracle : public OracleStorage<256>
{
public:
    PickySIMDOracle(long& expected_eval_size)
        : expected_eval_size(expected_eval_size)
    {
        // Nothing to do here
    }

    // We don't care about any of these functions
    void evalInterval(Interval::I&, bool&) override {}
    void evalPoint(float&, size_t) override {}
    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic>) override {}
    void evalFeatures(
            boost::container::small_vector<Feature, 4>&) override {}

    void evalArray(
            Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                                      LIBFIVE_EVAL_ARRAY_SIZE,
                                      Eigen::RowMajor>,
                         1, Eigen::Dynamic> out) override
    {
        REQUIRE(out.cols() == expected_eval_size);
        out = 0.0f;
    }

    void evalDerivArray(
            Eigen::Block<Eigen::Array<float, 3, LIBFIVE_EVAL_ARRAY_SIZE>,
                         3, Eigen::Dynamic, true> out) override
    {
        REQUIRE(out.cols() == expected_eval_size);
        out = 0.0f;
    }

    long& expected_eval_size;
};

class PickySIMDOracleClause : public OracleClause
{
public:
    PickySIMDOracleClause(long& expected_eval_size)
        : expected_eval_size(expected_eval_size)
    {
        // Nothing to do here
    }
    std::unique_ptr<Oracle> getOracle() const override
    {
        return std::unique_ptr<Oracle>(new PickySIMDOracle(expected_eval_size));
    }

    std::string name() const override
    {
        return "PickySIMDOracle";
    }
    long& expected_eval_size;
};

TEST_CASE("Oracle: check SIMD clamping")
{
    long expected_eval_size = 0;
    Tree picky(std::unique_ptr<PickySIMDOracleClause>(
            new PickySIMDOracleClause(expected_eval_size)));

    DerivArrayEvaluator eval(picky + Tree::X() + 1.0f);
    for (unsigned i=1; i < 100; ++i) {
        eval.set(Eigen::Vector3f(i, 0, 0), i);
        expected_eval_size = i;
        eval.values(i);
        eval.derivs(i);
    }

    // The test will have failed in PickySIMDOracle
    REQUIRE(true);
}
