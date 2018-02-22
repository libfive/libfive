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
#include <cmath>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_deriv_array.hpp"

using namespace Kernel;

TEST_CASE("DerivArrayEvaluator::deriv")
{
    SECTION("Every operator")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            auto tape = std::make_shared<Tape>(t);
            DerivArrayEvaluator e(tape);
            e.deriv({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("var + 2*X")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Tape>(v + 2 * Tree::X());
        DerivArrayEvaluator e(t, {{v.id(), 0}});

        auto out = e.deriv({2, 0, 0});
        REQUIRE(out.col(0) == Eigen::Vector4f(2, 0, 0, 4));
    }
}

TEST_CASE("DerivArrayEvaluator::derivs")
{
    SECTION("X")
    {
        auto t = std::make_shared<Tape>(Tree::X());
        DerivArrayEvaluator e(t);
        e.set({0, 0, 0}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 0, 0));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 0, 1));
    }

    SECTION("X + Z")
    {
        auto t = std::make_shared<Tape>(Tree::X() + Tree::Z());
        DerivArrayEvaluator e(t);

        e.set({1, 1, 1}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 1, 2));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 1, 4));
    }

    SECTION("X^(1/3)")
    {
        auto t = std::make_shared<Tape>(nth_root(Tree::X(), 3));
        DerivArrayEvaluator e(t);

        e.set({0, 0, 0}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        CAPTURE(d.col(0));
        REQUIRE(std::isinf(d.col(0)(0)));
        REQUIRE(d.col(0).bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 0));

        CAPTURE(d.col(1));
        REQUIRE(d.col(1)(0) == Approx(0.33333));
        REQUIRE(d.col(1).bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 1));
    }
}
