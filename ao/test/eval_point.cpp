/*
Ao: a CAD kernel for modeling with implicit functions
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
#include <Eigen/Geometry>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_point.hpp"

using namespace Kernel;

// These are the same tests that are run on the ArrayEvaluator
TEST_CASE("PointEvaluator::eval")
{
    SECTION("X")
    {
        auto t = std::make_shared<Tape>(Tree::X());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        auto t = std::make_shared<Tape>(Tree::Y());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        auto t = std::make_shared<Tape>(Tree(3.14));
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Tape>(v);
        PointEvaluator e(t, {{v.id(), 3.14}});
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        auto t = std::make_shared<Tape>(Tree::X() + 1);
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        auto t = std::make_shared<Tape>(Tree::X() + Tree::Z());
        PointEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        auto t = std::make_shared<Tape>(nth_root(Tree::X(), 3));
        PointEvaluator e(t);
        REQUIRE(e.eval({-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            auto p = std::make_shared<Tape>(t);
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

    auto t = std::make_shared<Tape>(a*1 + b*2 + c*3);
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
    auto t = std::make_shared<Tape>(min(Tree::X(), Tree::Y()));
    PointEvaluator e(t);

    e.evalAndPush({-1, 0, 0}); // specialize to just "X"
    REQUIRE(e.eval({-2, 0, 0}) == -2);
    REQUIRE(e.eval({4, 0, 0}) == 4);
    REQUIRE(e.eval({4, 5, 0}) == 4);
    REQUIRE(e.eval({10, 5, 0}) == 10);

    e.pop();
    e.evalAndPush({0, -1, 0}); // specialize to just "Y"
    REQUIRE(e.eval({-2, 0, 0}) == 0);
    REQUIRE(e.eval({4, 0, 0}) == 0);
    REQUIRE(e.eval({4, 5, 0}) == 5);
    REQUIRE(e.eval({10, 5, 0}) == 5);
}
