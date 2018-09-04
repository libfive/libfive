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
#include "libfive/eval/deck.hpp"

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
        auto t = std::make_shared<Deck>(Tree::X());
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        auto t = std::make_shared<Deck>(Tree::Y());
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        auto t = std::make_shared<Deck>(Tree(3.14));
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Deck>(v);
        ArrayEvaluator e(t, {{v.id(), 3.14}});
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        auto t = std::make_shared<Deck>(Tree::X() + 1);
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        auto t = std::make_shared<Deck>(Tree::X() + Tree::Z());
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        auto t = std::make_shared<Deck>(nth_root(Tree::X(), 3));
        ArrayEvaluator e(t);
        REQUIRE(eval(e, {-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            auto p = std::make_shared<Deck>(t);
            ArrayEvaluator e(p);
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

    auto t = std::make_shared<Deck>(a*1 + b*2 + c*3);
    ArrayEvaluator e(t, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});
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
    auto t = std::make_shared<Deck>(min(Tree::X(), -Tree::X()));
    ArrayEvaluator e(t);
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
