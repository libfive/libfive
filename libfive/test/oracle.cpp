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

#include "libfive/eval/oracle_storage.hpp"
#include "libfive/tree/oracle_clause.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

// This oracle wraps the X, Y, or Z axis
template <int A>
class AxisOracle : public OracleStorage<>
{
    void evalInterval(Interval::I& out) override
    {
        out = {lower(A), upper(A)};
    }

    void evalPoint(float& out, size_t index) override
    {
        out = points(index, A);
    }

    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic> /* out */) override
    {
        // Nothing to do here
    }

    void evalFeatures(
            boost::container::small_vector<Feature, 4>& out) override
    {
        Eigen::Vector3f v = Eigen::Vector3f::Zero();
        v(A) = 1;
        out.push_back(Feature(v));
    }
};

// Oracle clause for a particular axis, constructing AxisOracle objects
template <int A>
class AxisOracleClause : public OracleClause
{
    std::unique_ptr<Oracle> getOracle() const override
    {
        return std::unique_ptr<Oracle>(new AxisOracle<A>());
    }
};

// Replaces X, Y, and Z with oracles that pretend to be them
Tree convertToOracleAxes(Tree t)
{
    return t.remap(
            Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<0>)),
            Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<1>)),
            Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<2>)));
}

// Compares two BRep objects using Catch macros
template <unsigned N>
void BRepCompare(const BRep<N>& first, const BRep<N>& second)
{
    REQUIRE(first.verts.size() == second.verts.size());
    for (unsigned i = 0; i < first.verts.size(); ++i) {
        CAPTURE(i);
        CAPTURE(first.verts[i]);
        CAPTURE(second.verts[i]);
        REQUIRE(first.verts[i] == second.verts[i]);
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

  auto mesh = Mesh::render(sOracle, r);
  auto comparisonMesh = Mesh::render(s, r);

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

  auto mesh = Mesh::render(cubeOracle, r);
  auto comparisonMesh = Mesh::render(cube, r);

  BRepCompare(*mesh, *comparisonMesh);
}

class CubeOracle : public OracleStorage<>
{
    void evalInterval(Interval::I& out) override
    {
        using namespace boost::numeric; // for max

        Interval::I X(lower.x(), upper.x());
        Interval::I Y(lower.y(), upper.y());
        Interval::I Z(lower.z(), upper.z());

        out = max(max(
            max(-(X + 1.5f), X - 1.5f),
            max(-(Y + 1.5f), Y - 1.5f)),
            max(-(Z + 1.5f), Z - 1.5f));
    }

    void evalPoint(float& out, size_t index) override
    {
        float x = points(0, index);
        float y = points(1, index);
        float z = points(2, index);

        out = fmax(fmax(
            fmax(-(x + 1.5f), x - 1.5f),
            fmax(-(y + 1.5f), y - 1.5f)),
            fmax(-(z + 1.5f), z - 1.5f));
    }

    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic> out) override
    {
        out = out ||
            (points.leftCols(out.cols()).row(0).cwiseAbs() ==
             points.leftCols(out.cols()).row(1)).cwiseAbs() ||
            (points.leftCols(out.cols()).row(0).cwiseAbs() ==
             points.leftCols(out.cols()).row(2)).cwiseAbs() ||
            (points.leftCols(out.cols()).row(1).cwiseAbs() ==
             points.leftCols(out.cols()).row(2).cwiseAbs());
    }

    void evalFeatures(
            boost::container::small_vector<Feature, 4>& out) override
    {
        // We don't properly push epsilons, but that's okay for this
        // basic test (where we don't encounter other features).
        auto push = [&](Eigen::Vector3f d){ out.push_back(Feature(d)); };

        Eigen::Vector3f p = points.col(0);
        if (fabs(p.x()) >= fmax(fabs(p.y()), fabs(p.z())))
        {
            if (p.x() >= 0.f)
            {
                push({ 1.f, 0.f, 0.f });
            }
            if (p.x() <= 0.f)
            {
                push({ -1.f, 0.f, 0.f });
            }
        }
        if (fabs(p.y()) >= fmax(fabs(p.x()), fabs(p.z())))
        {
            if (p.y() >= 0.f)
            {
                push({ 0.f, 1.f, 0.f });
            }
            if (p.y() <= 0.f)
            {
                push({ 0.f, -1.f, 0.f });
            }
        }
        if (fabs(p.z()) >= fmax(fabs(p.y()), fabs(p.x())))
        {
            if (p.z() >= 0.f)
            {
                push({ 0.f, 0.f, 1.f });
            }
            if (p.z() <= 0.f)
            {
                push({ 0.f, 0.f, -1.f });
            }
        }
    }
};

class CubeOracleClause : public OracleClause
{
    std::unique_ptr<Oracle> getOracle() const override
    {
        return std::unique_ptr<Oracle>(new CubeOracle());
    }
};

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
