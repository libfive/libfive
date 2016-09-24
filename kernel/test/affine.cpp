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

#include "ao/kernel/tree/token.hpp"
#include "glm.hpp"

TEST_CASE("Affine math")
{
    SECTION("Affine plus constant")
    {
        Token t = Token::operation(Opcode::ADD, Token::affine(1, 0, 0, 0), Token::constant(1));

        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(1, 0, 0, 1));
    }

    SECTION("Constant plus affine")
    {
        Token t = Token::operation(Opcode::ADD, Token::constant(2), Token::affine(1, 0, 0, 0));

        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(1, 0, 0, 2));
    }

    SECTION("Affine plus affine")
    {
        Token t = Token::operation(Opcode::ADD, Token::affine(1, 2, 3, 4),
                                       Token::affine(2, 4, 6, 8));
        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(3, 6, 9, 12));
    }

    SECTION("Affine minus constant")
    {
        Token t = Token::operation(Opcode::SUB, Token::affine(1, 0, 0, 0), Token::constant(1));

        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(1, 0, 0, -1));
    }

    SECTION("Constant minus affine")
    {
        Token t = Token::operation(Opcode::SUB, Token::constant(2), Token::affine(1, 0, 0, 0));

        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(-1, 0, 0, 2));
    }

    SECTION("Affine minus affine")
    {
        Token t = Token::operation(Opcode::SUB, Token::affine(1, 2, 3, 4),
                                       Token::affine(2, 4, 6, 8));
        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(-1, -2, -3, -4));
    }

    SECTION("Affine times constant")
    {
        Token t = Token::operation(Opcode::MUL, Token::affine(1, 2, 3, 4), Token::constant(2));
        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(2, 4, 6, 8));
    }

    SECTION("Constant times affine")
    {
        Token t = Token::operation(Opcode::MUL, Token::constant(2), Token::affine(1, 2, 3, 4));
        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(2, 4, 6, 8));
    }

    SECTION("Affine divided by constant")
    {
        Token t = Token::operation(Opcode::DIV, Token::affine(1, 2, 3, 4), Token::constant(2));
        REQUIRE(t.opcode() == Opcode::AFFINE_VEC);
        REQUIRE(t.getAffine() == glm::vec4(0.5, 1, 1.5, 2));
    }
}

TEST_CASE("Token::collapse")
{
    SECTION("Base case")
    {
        auto t = Token::affine(1, 0, 0, 0);
        auto t_ = t.collapse();
        REQUIRE(t_.opcode() == Opcode::VAR_X);
        REQUIRE(t_.rank() == 0);
    }

    SECTION("Sum")
    {
        auto t = Token::affine(1, 1, 0, 0);
        auto t_ = t.collapse();
        REQUIRE(t_.opcode() == Opcode::ADD);
        REQUIRE(t_.lhs().opcode() == Opcode::VAR_X);
        REQUIRE(t_.rhs().opcode() == Opcode::VAR_Y);
        REQUIRE(t_.rank() == 1);
    }

    SECTION("Multiplication")
    {
        auto t = Token::affine(2, 3, 0, 0);
        auto t_ = t.collapse();
        REQUIRE(t_.opcode() == Opcode::ADD);
        REQUIRE(t_.lhs().opcode() == Opcode::MUL);
        REQUIRE(t_.rhs().opcode() == Opcode::MUL);

        REQUIRE(t_.lhs().lhs().opcode() == Opcode::VAR_X);
        REQUIRE(t_.lhs().rhs().opcode() == Opcode::CONST);
        REQUIRE(t_.lhs().rhs().value() == 2);
        REQUIRE(t_.rhs().lhs().opcode() == Opcode::VAR_Y);
        REQUIRE(t_.rhs().rhs().opcode() == Opcode::CONST);
        REQUIRE(t_.rhs().rhs().value() == 3);
    }

    SECTION("Constant")
    {
        auto t = Token::affine(1, 0, 0, 3);
        auto t_ = t.collapse();
        REQUIRE(t_.opcode() == Opcode::ADD);
        REQUIRE(t_.lhs().opcode() == Opcode::VAR_X);
        REQUIRE(t_.rhs().opcode() == Opcode::CONST);
        REQUIRE(t_.rhs().value() == 3);
    }

    SECTION("Not affine at all")
    {
        auto t = Token::X();
        auto t_ = t.collapse();
        REQUIRE(t.id == t_.id);
    }
}
