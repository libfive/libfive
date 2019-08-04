/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "catch.hpp"

#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/oracle/transformed_oracle_clause.hpp"
#include "libfive/oracle/transformed_oracle.hpp"
#include "libfive/eval/deck.hpp"

#include "util/shapes.hpp"
#include "util/oracles.hpp"

using namespace libfive;


TEST_CASE("OracleContext: TransformedOracle evaluation")
{
    auto cube = Tree(std::unique_ptr<const OracleClause>(new CubeOracleClause));
    auto t = Tree(std::unique_ptr<const OracleClause>(
                new TransformedOracleClause(cube,
                    min(Tree::X(), Tree::X() + Tree::Y()),
                    Tree::Z(), Tree::Y())));

    Region<3> r({-4, -4, -4}, {4, 4, 4});

    BRepSettings settings;
    settings.min_feature = 0.1;
    auto mesh = Mesh::render(t, r, settings);
    REQUIRE(true);
}

TEST_CASE("OracleContext: TransformedOracle push/pop")
{
    // Construct a tree that is min(X, Y), but done with oracles
    auto cube = Tree(std::unique_ptr<const OracleClause>(new AxisOracleClause<0>));
    auto remapped = cube.remap(min(Tree::X(), Tree::Y()), Tree::Y(), Tree::Z());

    auto deck = std::make_shared<Deck>(remapped);
    Evaluator p(deck);
    REQUIRE(p.value({1.0, 2.0, 3.0}) == 1.0);
    REQUIRE(p.value({1.0, 0.0, 3.0}) == 0.0);

    // Do an interval evaluation that selects the X branch of min(X, Y)
    IntervalEvaluator i(deck);
    auto o = i.intervalAndPush({1.0, 2.0, 3.0}, {1.5, 2.5, 3.5});
    REQUIRE(o.first.lower() == 1.0);
    REQUIRE(o.first.upper() == 1.5);

    // Check to make sure that the X branch is selected in the tape
    REQUIRE(p.value({1.0, 2.0, 3.0}, *o.second) == 1.0);
    REQUIRE(p.value({1.0, 0.0, 3.0}, *o.second) == 1.0);
}
