/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <catch/catch.hpp>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Variable evaluation")
{
    Evaluator e(Tree::X());

    REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Float evaluation")
{
    Evaluator e(Tree::operation(Opcode::ADD, Tree::X(), Tree::constant(1)));
    REQUIRE(e.eval(1.0, 2.0, 3.0) == 2.0);
}

TEST_CASE("Interval evaluation")
{
    Evaluator e(Tree::operation(Opcode::ADD, Tree::X(), Tree::constant(1)));

    Interval arg(1, 2);
    auto out = e.eval(arg, arg, arg);

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("Push / pop behavior")
{
    Evaluator e(
        Tree::operation(Opcode::MIN,
            Tree::operation(Opcode::ADD, Tree::X(), Tree::constant(1)),
            Tree::operation(Opcode::ADD, Tree::Y(), Tree::constant(1))));

    // Store -3 in the rhs's value
    REQUIRE(e.eval(1.0f, -3.0f, 0.0f) == -2);

    // Do an interval evaluation that will lead to disabling the rhs
    auto i = e.eval(Interval(-5, -4), Interval(8, 9), Interval(0, 0));
    REQUIRE(i.lower() == -4);
    REQUIRE(i.upper() == -3);

    // Push (which should disable the rhs of min
    e.push();

    // Check to make sure that the push disabled something
    CAPTURE(e.utilization());
    REQUIRE(e.utilization() < 1);

    // Require that the evaluation gets 1
    REQUIRE(e.eval(1.0f, 2.0f, 0.0f) == 2);
}
