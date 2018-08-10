/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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
#include "libfive/oracle/transformed_oracle_clause.hpp"
#include "libfive/oracle/transformed_oracle.hpp"
#include "libfive/eval/deck.hpp"

#include "util/shapes.hpp"
#include "util/oracles.hpp"

using namespace Kernel;


TEST_CASE("OracleContext: TransformedOracle evaluation")
{
    auto cube = Tree(std::unique_ptr<const OracleClause>(new CubeOracleClause));
    auto t = Tree(std::unique_ptr<const OracleClause>(
                new TransformedOracleClause(cube,
                    min(Tree::X(), Tree::X() + Tree::Y()),
                    Tree::Z(), Tree::Y())));

    Region<3> r({-4, -4, -4}, {4, 4, 4});
    auto mesh = Mesh::render(t, r, 0.1);
    REQUIRE(true);
}

TEST_CASE("OracleContext: TransformedOracle push/pop")
{
    // Construct a tree that is min(X, Y), but done with oracles
    auto cube = Tree(std::unique_ptr<const OracleClause>(new AxisOracleClause<0>));
    auto remapped = cube.remap(min(Tree::X(), Tree::Y()), Tree::Y(), Tree::Z());

    auto deck = std::make_shared<Deck>(remapped);
    PointEvaluator p(deck);
    REQUIRE(p.eval({1.0, 2.0, 3.0}) == 1.0);
    REQUIRE(p.eval({1.0, 0.0, 3.0}) == 0.0);

    // Do an interval evaluation that selects the X branch of min(X, Y)
    IntervalEvaluator i(deck);
    auto o = i.evalAndPush({1.0, 2.0, 3.0}, {1.5, 2.5, 3.5});
    REQUIRE(o.first.lower() == 1.0);
    REQUIRE(o.first.upper() == 1.5);

    // Check to make sure that the X branch is selected in the tape
    REQUIRE(p.eval({1.0, 2.0, 3.0}, o.second) == 1.0);
    REQUIRE(p.eval({1.0, 0.0, 3.0}, o.second) == 1.0);
}
