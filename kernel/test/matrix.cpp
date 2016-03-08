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

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Matrix evaluation")
{
    Store s;
    Tree t(&s, s.X());
    Evaluator e(&t);

    SECTION("Default matrix")
    {
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        e.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Swapping")
    {
        e.setMatrix(glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == Approx(2.0));
    }

    SECTION("Re-evaluation")
    {
        e.eval(1.0, 2.0, 3.0);
        e.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Offset")
    {
        e.setMatrix(glm::translate(glm::mat4(), {0.5, 0.0, 0.0}));
        REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.5);
    }
}
