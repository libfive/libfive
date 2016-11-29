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

#include <glm/gtc/matrix_transform.hpp>
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Principle variable evaluation")
{
    Evaluator e(Tree::X());

    REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Constant evaluation")
{
    Evaluator e(Tree(3.14));
    REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(3.14));
}

TEST_CASE("Secondary variable evaluation")
{
    Evaluator e(Tree::var(3.14));
    REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(3.14));
}

TEST_CASE("Evaluator::gradient")
{
    SECTION("constant + variable")
    {
        auto v = Tree::var(3.14);
        Evaluator e(Tree(Opcode::ADD, v, Tree(1.0)));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(4.14));
        auto g = e.gradient(1, 2, 3);
        REQUIRE(g.size() == 1);
        REQUIRE(g.count(v.var()) == 1);
        REQUIRE(g.at(v.var()) == Approx(1));
    }

    SECTION("x * variable")
    {
        auto v = Tree::var(1.0);
        Evaluator e(Tree(Opcode::MUL, Tree::X(), v));
        {
            auto g = e.gradient(2, 0, 0);
            REQUIRE(g.size() == 1);
            REQUIRE(g.at(v.var()) == Approx(2));
        }
        {
            auto g = e.gradient(3, 0, 0);
            REQUIRE(g.at(v.var()) == Approx(3));
        }
    }

    SECTION("Multiple variables")
    {
        // Deliberately construct out of order
        auto a = Tree::var(3.0);
        auto c = Tree::var(7.0);
        auto b = Tree::var(5.0);

        Evaluator e(Tree(Opcode::ADD, Tree(Opcode::ADD,
            Tree(Opcode::MUL, Tree(1.0f), a),
            Tree(Opcode::MUL, Tree(2.0f), b)),
            Tree(Opcode::MUL, Tree(3.0f), c)));
        REQUIRE(e.eval(0, 0, 0) == Approx(34));
        auto g = e.gradient(0, 0, 0);
        REQUIRE(g.at(a.var()) == Approx(1.0f));
        REQUIRE(g.at(b.var()) == Approx(2.0f));
        REQUIRE(g.at(c.var()) == Approx(3.0f));
    }
}

TEST_CASE("Evaluator::setVar")
{
    // Deliberately construct out of order
    auto a = Tree::var(3.0);
    auto c = Tree::var(7.0);
    auto b = Tree::var(5.0);

    Evaluator e(Tree(Opcode::ADD, Tree(Opcode::ADD,
        Tree(Opcode::MUL, Tree(1.0f), a),
        Tree(Opcode::MUL, Tree(2.0f), b)),
        Tree(Opcode::MUL, Tree(3.0f), c)));
    REQUIRE(e.eval(0, 0, 0) == Approx(34));

    e.setVar(a.var(), 5);
    REQUIRE(e.eval(0, 0, 0) == Approx(36));
    e.setVar(b.var(), 0);
    REQUIRE(e.eval(0, 0, 0) == Approx(26));
    e.setVar(c.var(), 10);
    REQUIRE(e.eval(0, 0, 0) == Approx(35));
}

TEST_CASE("Float evaluation")
{
    SECTION("X + 1")
    {
        Evaluator e(Tree(Opcode::ADD, Tree::X(), Tree(1)));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 2.0);
    }

    SECTION("X + Z")
    {
        Evaluator e(Tree(Opcode::ADD, Tree::X(), Tree::Z()));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 4.0);
    }
}

TEST_CASE("Interval evaluation")
{
    Evaluator e(Tree(Opcode::ADD, Tree::X(), Tree(1)));

    Interval arg(1, 2);
    auto out = e.eval(arg, arg, arg);

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("Push / pop behavior")
{
    Evaluator e(
        Tree(Opcode::MIN,
            Tree(Opcode::ADD, Tree::X(), Tree(1)),
            Tree(Opcode::ADD, Tree::Y(), Tree(1))));

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

TEST_CASE("Affine evaluation")
{
    Evaluator e(Tree::affine(1, 0, 0, 0));
    REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Matrix evaluation")
{
    Tree t(Tree::affine(1, 0, 0, 0));

    SECTION("Default matrix")
    {
        Evaluator e(t);
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        Evaluator e(t, glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Swapping")
    {
        Evaluator e(t, glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(2.0));
    }

    SECTION("Offset")
    {
        Evaluator e(t, glm::translate(glm::mat4(), {0.5, 0.0, 0.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.5);
    }
}

TEST_CASE("Matrix normals")
{
    Tree t(Tree::affine(1, 0, 0, 0));

    SECTION("Swapping")
    {
        Evaluator e(t, glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        e.set(1, 2, 3, 0);
        auto out = e.derivs(1);
        glm::vec3 d(std::get<1>(out)[0],
                    std::get<2>(out)[0],
                    std::get<3>(out)[0]);

        REQUIRE(d.x == Approx(0));
        REQUIRE(d.y == Approx(1));
        REQUIRE(d.z == Approx(0));
    }

    SECTION("Swapping")
    {
        Evaluator e(t, glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        e.set(1, 2, 3, 0);
        auto out = e.derivs(1);
        glm::vec3 d(std::get<1>(out)[0],
                    std::get<2>(out)[0],
                    std::get<3>(out)[0]);

        REQUIRE(d.x == Approx(0));
        REQUIRE(d.y == Approx(1));
        REQUIRE(d.z == Approx(0));
    }
}

TEST_CASE("Evaluator::derivs")
{
    SECTION("X")
    {
        Evaluator e(Tree::X());
        e.set(0, 0, 0, 0);
        e.set(1, 2, 3, 1);
        auto d = e.derivs(2);

        // Values = x
        REQUIRE(std::get<0>(d)[0] == 0.0);
        REQUIRE(std::get<0>(d)[1] == 1.0);

        // d/dx = 1
        REQUIRE(std::get<1>(d)[0] == 1.0);
        REQUIRE(std::get<1>(d)[1] == 1.0);

        // d/dy = 0
        REQUIRE(std::get<2>(d)[0] == 0.0);
        REQUIRE(std::get<2>(d)[1] == 0.0);

        // d/dz = 0
        REQUIRE(std::get<3>(d)[0] == 0.0);
        REQUIRE(std::get<3>(d)[1] == 0.0);
    }

    SECTION("X + Z")
    {
        Evaluator e(Tree(Opcode::ADD, Tree::X(), Tree::Z()));

        e.set(1, 1, 1, 0);
        e.set(1, 2, 3, 1);
        auto d = e.derivs(2);

        // Values = x
        REQUIRE(std::get<0>(d)[0] == 2.0);
        REQUIRE(std::get<0>(d)[1] == 4.0);

        // d/dx = 1
        REQUIRE(std::get<1>(d)[0] == 1.0);
        REQUIRE(std::get<1>(d)[1] == 1.0);

        // d/dy = 0
        REQUIRE(std::get<2>(d)[0] == 0.0);
        REQUIRE(std::get<2>(d)[1] == 0.0);

        // d/dz = 1
        REQUIRE(std::get<3>(d)[0] == 1.0);
        REQUIRE(std::get<3>(d)[1] == 1.0);
    }
}
