/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_array.hpp"

using namespace Kernel;

float eval(ArrayEvaluator& a, Eigen::Vector3f pt)
{
    a.set(pt, 0);
    return a.values(1)(0);
}

TEST_CASE("ArrayEvaluator::eval")
{
    SECTION("X")
    {
        ArrayEvaluator e(Tree::X());
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        ArrayEvaluator e(Tree::Y());
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        ArrayEvaluator e(Tree(3.14));
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        ArrayEvaluator e(v, {{v.id(), 3.14}});
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        ArrayEvaluator e(Tree::X() + 1);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        ArrayEvaluator e(Tree::X() + Tree::Z());
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        ArrayEvaluator e(nth_root(Tree::X(), 3));
        REQUIRE(eval(e, {-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            ArrayEvaluator e(t);
            eval(e, {0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }
}

TEST_CASE("ArrayEvaluator::setVar")
{
    // Deliberately construct out of order
    auto a = Tree::var();
    auto c = Tree::var();
    auto b = Tree::var();

    ArrayEvaluator e(a*1 + b*2 + c*3, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});
    REQUIRE(eval(e, {0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(eval(e, {0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(eval(e, {0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(eval(e, {0, 0, 0}) == Approx(35));
}

TEST_CASE("ArrayEvaluator::getAmbiguous")
{
    ArrayEvaluator e(min(Tree::X(), -Tree::X()));
    e.set({0, 0, 0}, 0);
    e.set({1, 0, 0}, 1);
    e.set({2, 0, 0}, 2);
    e.set({0, 0, 0}, 3);

    e.values(4);

    auto a = e.getAmbiguous(3);
    REQUIRE(a.count() == 1);
    REQUIRE(a(0) == true);

    auto b = e.getAmbiguous(4);
    REQUIRE(b.count() == 2);
    REQUIRE(b(0) == true);
    REQUIRE(b(3) == true);
}
