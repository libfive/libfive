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
#include <cmath>

#include <catch/catch.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/kernel/tree/token.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Matrix evaluation")
{
    std::unique_ptr<Token> t(Token::affine(1, 0, 0, 0));

    SECTION("Default matrix")
    {
        Evaluator e(t.get());
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        Evaluator e(t.get(), glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Swapping")
    {
        Evaluator e(t.get(), glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(2.0));
    }

    SECTION("Offset")
    {
        Evaluator e(t.get(), glm::translate(glm::mat4(), {0.5, 0.0, 0.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.5);
    }
}

TEST_CASE("Matrix normals")
{
    std::unique_ptr<Token> t(Token::affine(1, 0, 0, 0));

    SECTION("Swapping")
    {
        Evaluator e(t.get(), glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
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
        Evaluator e(t.get(), glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
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
