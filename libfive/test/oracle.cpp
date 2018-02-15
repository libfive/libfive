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

TEST_CASE("Oracle::Render and compare (sphere)")
{
  Tree s = sphere(0.5);
  Region<3> r({ -1, -1, -1 }, { 1, 1, 1 });
  Tree sOracle = convertToOracleAxes(s);

  auto mesh = Mesh::render(sOracle, r);
  auto comparisonMesh = Mesh::render(s, r);

  REQUIRE(*mesh == *comparisonMesh);
}

TEST_CASE("Oracle::Render and compare (cube)")
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

  REQUIRE(*mesh == *comparisonMesh);
}

/*  In order to test handling of multiple-gradient points, a cube oracle
 *  is also created (ranging in each dimension from -1.5 to 1.5), and compared
 *  to an ordinary cube.
 */

namespace Kernel {

class cubeAsOracle : public Oracle
{
public:
    Interval::I getRange(Region<2> region) const override
    {
        auto minX = (region.lower.x() < 0 && region.upper.x() > 0)
            ? -1.5
            : std::min(abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto maxX = std::max(
            abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto minY = (region.lower.y() < 0 && region.upper.y() > 0)
            ? -1.5
            : std::min(abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto maxY = std::max(
            abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto z = abs(region.perp(0)) - 1.5;
        return { std::max({minX, minY, z}),std::max({ maxX, maxY, z }) };
    }

    Interval::I getRange(Region<3> region) const override
    {
        auto minX = (region.lower.x() < 0 && region.upper.x() > 0)
            ? -1.5
            : std::min(abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto maxX = std::max(
            abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto minY = (region.lower.y() < 0 && region.upper.y() > 0)
            ? -1.5
            : std::min(abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto maxY = std::max(
            abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto minZ = (region.lower.z() < 0 && region.upper.z() > 0)
            ? -1.5
            : std::min(abs(region.lower.z()) - 1.5, abs(region.upper.z()) - 1.5);
        auto maxZ = std::max(
            abs(region.lower.z()) - 1.5, abs(region.upper.z()) - 1.5);
        return { std::max({ minX, minY, minZ }),std::max({ maxX, maxY, maxZ }) };
    }

    boost::container::small_vector<std::pair<Eigen::Vector3f, Eigen::Vector3f>, 1>
        getGradients(Eigen::Vector3f point) const override
    {
        boost::container::small_vector<std::pair<Eigen::Vector3f, Eigen::Vector3f>, 1> out;
        if (abs(point.x()) <= 1.5f &&
            abs(point.y()) <= 1.5f &&
            abs(point.z()) <= 1.5f) //If it's inside or on the cube
        {
            if (abs(point.x()) >= std::max(abs(point.y()), abs(point.z()))) {
                if (point.x() >= 0.f)
                {
                    out.push_back({ { 1.f, 0.f, 0.f }, {1.f, 0.f, 0.f} });
                }
                if (point.x() <= 0.f)
                {
                    out.push_back({ { -1.f, 0.f, 0.f },{ -1.f, 0.f, 0.f } });
                }
            }
            if (abs(point.y()) >= std::max(abs(point.x()), abs(point.z()))) {
                if (point.y() >= 0.f)
                {
                    out.push_back({ { 0.f, 1.f, 0.f },{ 0.f, 1.f, 0.f } });
                }
                if (point.y() <= 0.f)
                {
                    out.push_back({ { 0.f, -1.f, 0.f },{ 0.f, -1.f, 0.f } });
                }
            }
            if (abs(point.z()) >= std::max(abs(point.y()), abs(point.x()))) {
                if (point.z() >= 0.f)
                {
                    out.push_back({ { 0.f, 0.f, 1.f },{ 0.f, 0.f, 1.f } });
                }
                if (point.z() <= 0.f)
                {
                    out.push_back({ { 0.f, 0.f, -1.f },{ 0.f, 0.f, -1.f } });
                }
            }
        }
        if (abs(point.x()) >= 1.5f ||
            abs(point.y()) >= 1.5f ||
            abs(point.z()) >= 1.5f) //If it's outside or on the cube
        {
            //Find the vector from the cube to the point.
            auto xDistance = (abs(point.x()) <= 1.5f)
                ? 0.f
                : std::min(
                    point.x() - 1.5f, point.x() + 1.5f,
                    [](float a, float b) {return abs(a) < abs(b); });
            auto yDistance = (abs(point.y()) <= 1.5f)
                ? 0.f
                : std::min(
                    point.y() - 1.5f, point.y() + 1.5f,
                    [](float a, float b) {return abs(a) < abs(b); });
            auto zDistance = (abs(point.z()) <= 1.5f)
                ? 0.f
                : std::min(
                    point.z() - 1.5f, point.z() + 1.5f,
                    [](float a, float b) {return abs(a) < abs(b); });
            Eigen::Vector3f totalVector(xDistance, yDistance, zDistance);
            out.push_back({ totalVector, totalVector });
        }
        return out;
    }

    float getValue(Eigen::Vector3f point) const override
    {
        return std::max({ abs(point.x()), abs(point.y()), abs(point.z()) }) - 1.5f;
    }
};
} //namespace Kernel

TEST_CASE("Oracle::Render and compare (cube as oracle)")
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

    REQUIRE(*mesh == *comparisonMesh);
}