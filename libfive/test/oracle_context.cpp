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
