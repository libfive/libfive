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
#include "ao/kernel/solve/solver.hpp"

TEST_CASE("Solver::findRoot")
{
    auto v = Tree::var(3);
    auto out = Solver::findRoot(v + 1);
    auto res = out.first;
    auto vals = out.second;
    REQUIRE(res == Approx(0));
    REQUIRE(vals.size() == 1);
    REQUIRE(vals.at(v.var()) == Approx(-1));
}
