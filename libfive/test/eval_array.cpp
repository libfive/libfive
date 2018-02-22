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
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_array.hpp"

using namespace Kernel;

TEST_CASE("ArrayEvaluator::eval")
{
    SECTION("X")
    {
        auto t = std::make_shared<Tape>(Tree::X());
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        auto t = std::make_shared<Tape>(Tree::Y());
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        auto t = std::make_shared<Tape>(Tree(3.14));
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Tape>(v);
        ArrayEvaluator e(t, {{v.id(), 3.14}}, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        auto t = std::make_shared<Tape>(Tree::X() + 1);
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        auto t = std::make_shared<Tape>(Tree::X() + Tree::Z());
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        auto t = std::make_shared<Tape>(nth_root(Tree::X(), 3));
        ArrayEvaluator e(t, 0);
        REQUIRE(e.eval({-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation that takes at least one argument")
    {
        for (unsigned i=0; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            if (Opcode::args(op) == 1 || Opcode::args(op) == 2)
            {
                Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                    : Tree(op, Tree::X()));
                auto p = std::make_shared<Tape>(t);
                ArrayEvaluator e(p, 0);
                e.eval({ 0, 0, 0 });
                REQUIRE(true /* No crash! */);
            }
        }
    }
}

TEST_CASE("ArrayEvaluator::setVar")
{
    // Deliberately construct out of order
    auto a = Tree::var();
    auto c = Tree::var();
    auto b = Tree::var();

    auto t = std::make_shared<Tape>(a*1 + b*2 + c*3);
    ArrayEvaluator e(t, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}}, 0);
    REQUIRE(e.eval({0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(e.eval({0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(e.eval({0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(e.eval({0, 0, 0}) == Approx(35));
}

TEST_CASE("ArrayEvaluator::evalAndPush")
{
    auto t = std::make_shared<Tape>(min(Tree::X(), Tree::Y()));
    ArrayEvaluator e(t, 0);

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

TEST_CASE("ArrayEvaluator::getAmbiguous")
{
    auto t = std::make_shared<Tape>(min(Tree::X(), -Tree::X()));
    ArrayEvaluator e(t, 0);
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
