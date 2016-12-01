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
    SECTION("Single variable")
    {
        auto v = Tree::var(3);
        auto out = Solver::findRoot(v + 1);
        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 1);
        REQUIRE(vals.at(v.var()) == Approx(-1));
    }

    SECTION("Single variable (negative)")
    {
        auto v = Tree::var(3);
        auto out = Solver::findRoot(1 - v);
        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 1);
        REQUIRE(vals.at(v.var()) == Approx(1));
    }

    SECTION("Multiple variables")
    {
        auto a = Tree::var(3);
        auto b = Tree::var(5);

        auto out = Solver::findRoot(a*a + b*b - 1);
        auto res = out.first;
        auto vals = out.second;

        // Here, we should walk down towards 0, so we'd expect both
        // variables to be scaled by the same size
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 2);
        REQUIRE(vals.at(a.var()) == Approx(0.5145));
        REQUIRE(vals.at(b.var()) == Approx(0.8575));
    }

    SECTION("Sum-of-squares performance")
    {
        // Constraint solving as sum-of-squares optimization
        // (based on an example at mattkeeter.com/projects/constraints)
        auto ax = Tree::var(-3);
        auto ay = Tree::var(3);
        auto bx = Tree::var(1);
        auto by = Tree::var(0);
        auto cx = Tree::var(3);
        auto cy = Tree::var(2);

        // Constraints
        auto r1 = square(ax) + square(ay) - 1;
        auto r2 = square(ax - bx) + square(ay - by) - 2;
        auto r3 = square(by);
        auto r4 = square(cy);
        auto r5 = cx - bx;

        auto sos = square(r1) + square(r2) + square(r3) + square(r4) + square(r5);

        std::chrono::time_point<std::chrono::system_clock> start, end;
        std::chrono::duration<double> elapsed;
        start = std::chrono::system_clock::now();
        auto out = Solver::findRoot(sos);
        end = std::chrono::system_clock::now();

        elapsed = end - start;
        auto elapsed_ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

        std::string log = "\nSolved SOS root in " +
               std::to_string(elapsed.count()) + " sec";
        WARN(log);

        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res < 1e-4);
        REQUIRE(vals.size() == 6);
    }
}
