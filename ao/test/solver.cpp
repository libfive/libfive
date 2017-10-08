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
#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/solve/solver.hpp"

using namespace Kernel;

TEST_CASE("Solver::findRoot")
{
    SECTION("Single variable")
    {
        auto v = Tree::var();
        auto out = Solver::findRoot(v + 1, {{v.id(), 3}});
        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 1);
        REQUIRE(vals.at(v.id()) == Approx(-1));
    }

    SECTION("Single variable (negative)")
    {
        auto v = Tree::var();
        auto out = Solver::findRoot(1 - v, {{v.id(), 3}});
        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 1);
        REQUIRE(vals.at(v.id()) == Approx(1));
    }

    SECTION("Multiple variables")
    {
        auto a = Tree::var();
        auto b = Tree::var();

        auto out = Solver::findRoot(a*a + b*b - 1, {{a.id(), 3}, {b.id(), 5}});
        auto res = out.first;
        auto vals = out.second;

        // Here, we should walk down towards 0, so we'd expect both
        // variables to be scaled by the same size
        REQUIRE(res == Approx(0));
        REQUIRE(vals.size() == 2);
        REQUIRE(vals.at(a.id()) == Approx(0.5145));
        REQUIRE(vals.at(b.id()) == Approx(0.8575));
    }

    SECTION("Mask")
    {
        auto a = Tree::var();
        auto b = Tree::var();

        auto err = a + b - 1;
        {
            auto out = Solver::findRoot(err, {{a.id(), 1}, {b.id(), 1}},
                    {0,0,0}, {a.id()});
            auto res = out.first;
            auto vals = out.second;
            REQUIRE(res == Approx(0));
            REQUIRE(vals.size() == 1);
            REQUIRE(vals.at(b.id()) == Approx(0));
        }

        {
            auto out = Solver::findRoot(err, {{a.id(), 1}, {b.id(), 1}},
                    {0,0,0}, {b.id()});
            auto res = out.first;
            auto vals = out.second;
            REQUIRE(res == Approx(0));
            REQUIRE(vals.size() == 1);
            REQUIRE(vals.at(a.id()) == Approx(0));
        }
    }

    SECTION("Sum-of-squares performance")
    {
        // Constraint solving as sum-of-squares optimization
        // (based on an example at mattkeeter.com/projects/constraints)
        auto ax = Tree::var();
        auto ay = Tree::var();
        auto bx = Tree::var();
        auto by = Tree::var();
        auto cx = Tree::var();
        auto cy = Tree::var();

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
        auto out = Solver::findRoot(sos,
                {{ax.id(), -3}, {ay.id(), 3},
                 {bx.id(), 1}, {by.id(), 0},
                 {cx.id(), 3}, {cy.id(), 2}});
        end = std::chrono::system_clock::now();

        elapsed = end - start;
        auto elapsed_ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

        std::string log = "\nSolved SOS root in " +
               std::to_string(elapsed.count()) + " sec";
        WARN(log);

        auto res = out.first;
        auto vals = out.second;
        REQUIRE(res < 1e-3);
        REQUIRE(vals.size() == 6);
    }
}
