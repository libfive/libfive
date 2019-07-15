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

TEST_CASE("ArrayEvaluator::eval")
{
    SECTION("X")
    {
        ArrayEvaluator e(Tree::X());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        ArrayEvaluator e(Tree::Y());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        ArrayEvaluator e(Tree(3.14));
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        ArrayEvaluator e(v, {{v.id(), 3.14}});
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Constant + variable")
    {
        auto v = Tree::var();
        ArrayEvaluator e(v + 1.0, {{v.id(), 3.14}});
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(4.14));
    }

    SECTION("Multiple variables")
    {
        // Deliberately construct out of order
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        ArrayEvaluator e(a*1 + b*2 + c*3,
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        REQUIRE(e.value({0, 0, 0}) == Approx(34));
    }

    SECTION("X + 1")
    {
        ArrayEvaluator e(Tree::X() + 1);
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        ArrayEvaluator e(Tree::X() + Tree::Z());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        ArrayEvaluator e(nth_root(Tree::X(), 3));
        REQUIRE(e.value({-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            ArrayEvaluator e(t);
            e.value({0, 0, 0});
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
    REQUIRE(e.value({0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(e.value({0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(e.value({0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(e.value({0, 0, 0}) == Approx(35));
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

TEST_CASE("ArrayEvaluator::values (returned size)")
{
    ArrayEvaluator e(min(Tree::X(), -Tree::X()));
    e.set({0, 0, 0}, 0);
    e.set({1, 0, 0}, 1);
    e.set({2, 0, 0}, 2);

    auto o = e.values(3);
    REQUIRE(o.cols() == 3);
}

TEST_CASE("ArrayEvaluator::valueAndPush")
{
    ArrayEvaluator e(min(Tree::X(), Tree::Y()));

    {
        auto h = e.valueAndPush({-1, 0, 0}); // specialize to just "X"
        REQUIRE(e.value({-2, 0, 0}, *h.second) == -2);
        REQUIRE(e.value({4, 0, 0}, *h.second) == 4);
        REQUIRE(e.value({4, 5, 0}, *h.second) == 4);
        REQUIRE(e.value({10, 5, 0}, *h.second) == 10);
    }

    {
        auto h = e.valueAndPush({0, -1, 0}); // specialize to just "Y"
        REQUIRE(e.value({-2, 0, 0}, *h.second) == 0);
        REQUIRE(e.value({4, 0, 0}, *h.second) == 0);
        REQUIRE(e.value({4, 5, 0}, *h.second) == 5);
        REQUIRE(e.value({10, 5, 0}, *h.second) == 5);
    }
}
