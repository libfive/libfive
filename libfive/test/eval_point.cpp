/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017-2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_point.hpp"
#include "libfive/eval/deck.hpp"

using namespace Kernel;

// These are the same tests that are run on the ArrayEvaluator
TEST_CASE("PointEvaluator::eval")
{
    SECTION("X")
    {
        auto t = std::make_shared<Deck>(Tree::X());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        auto t = std::make_shared<Deck>(Tree::Y());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        auto t = std::make_shared<Deck>(Tree(3.14));
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Deck>(v);
        PointEvaluator e(t, {{v.id(), 3.14}});
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        auto t = std::make_shared<Deck>(Tree::X() + 1);
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        auto t = std::make_shared<Deck>(Tree::X() + Tree::Z());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        auto t = std::make_shared<Deck>(nth_root(Tree::X(), 3));
        PointEvaluator e(t);
        REQUIRE(e.eval({-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            auto p = std::make_shared<Deck>(t);
            PointEvaluator e(p);
            e.eval({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }
}

TEST_CASE("PointEvaluator::setVar")
{
    // Deliberately construct out of order
    auto a = Tree::var();
    auto c = Tree::var();
    auto b = Tree::var();

    auto t = std::make_shared<Deck>(a*1 + b*2 + c*3);
    PointEvaluator e(t, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});
    REQUIRE(e.eval({0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(e.eval({0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(e.eval({0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(e.eval({0, 0, 0}) == Approx(35));
}

TEST_CASE("PointEvaluator::evalAndPush")
{
    auto t = std::make_shared<Deck>(min(Tree::X(), Tree::Y()));
    PointEvaluator e(t);

    {
        auto h = e.evalAndPush({-1, 0, 0}); // specialize to just "X"
        REQUIRE(e.eval({-2, 0, 0}, h.second) == -2);
        REQUIRE(e.eval({4, 0, 0}, h.second) == 4);
        REQUIRE(e.eval({4, 5, 0}, h.second) == 4);
        REQUIRE(e.eval({10, 5, 0}, h.second) == 10);
    }

    {
        auto h = e.evalAndPush({0, -1, 0}); // specialize to just "Y"
        REQUIRE(e.eval({-2, 0, 0}, h.second) == 0);
        REQUIRE(e.eval({4, 0, 0}, h.second) == 0);
        REQUIRE(e.eval({4, 5, 0}, h.second) == 5);
        REQUIRE(e.eval({10, 5, 0}, h.second) == 5);
    }
}
