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
#include "libfive/eval/eval_interval.hpp"
#include "libfive/eval/eval_point.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/render/brep/region.hpp"

using namespace Kernel;

TEST_CASE("IntervalEvaluator::eval")
{
    SECTION("Basic math")
    {
        auto t = std::make_shared<Deck>(Tree::X() + 1);
        IntervalEvaluator e(t);

        auto out = e.eval({1,1,1}, {2,2,2});

        REQUIRE(out.lower() == 2.0);
        REQUIRE(out.upper() == 3.0);
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            auto p = std::make_shared<Deck>(t);
            IntervalEvaluator e(p);
            e.eval({0, 0, 0}, {1, 1, 1});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("Bounds growth")
    {
        IntervalEvaluator e(std::make_shared<Deck>(
            (Tree::X() + Tree::Y()) * (Tree::X() - Tree::Y())));

        auto o = e.eval({0, 0, 0}, {1, 1, 1});
        REQUIRE(o.lower() == -2);
        REQUIRE(o.upper() == 2);
    }
}

TEST_CASE("IntervalEvaluator::evalAndPush")
{
    SECTION("Basic")
    {
        auto t = std::make_shared<Deck>(
                min(Tree::X() + 1, Tree::Y() + 1));
        IntervalEvaluator e(t);

        // Store -3 in the rhs's value
        auto o = e.eval({1, -3, 0}, {1, -3, 0});
        REQUIRE(o.lower() == -2);
        REQUIRE(o.upper() == -2);

        // Do an interval evaluation that will lead to disabling the rhs
        // Pushing should disable the rhs of min
        auto p = e.evalAndPush({-5, 8, 0}, {-4, 9, 0});
        auto i = p.first;
        REQUIRE(i.lower() == -4);
        REQUIRE(i.upper() == -3);

        // Check to make sure that the push disabled something
        REQUIRE(p.second->size() < t->tape->size());

        // Require that the evaluation gets 1
        o = e.eval({1, 2, 0}, {1, 2, 0});
        REQUIRE(o.lower() == 2);
        REQUIRE(o.upper() == 2);
    }

    SECTION("Multi-min trees")
    {
        auto t = std::make_shared<Deck>(min(Tree::X(), min(
                min(Tree::X(), Tree::Y()),
                min(Tree::X(), Tree::Y() + 3))));

        IntervalEvaluator e(t);

        // Do an interval evaluation that should lead to both sides
        // picking X, then collapsing min(X, X) into just X.
        auto i = e.evalAndPush({-5, 0, 0}, {-4, 1, 0});
        CAPTURE(i.second->size());
        CAPTURE(t->tape->size());

        auto ratio = i.second->size() / (float)t->tape->size();
        REQUIRE(ratio == Approx(1/5.0));
    }

    SECTION("With NaNs")
    {
        auto x = Tree::X();
        auto y = Tree::Y();
        auto r = sqrt(x*x + y*y);
        auto t = atan(y / x);
        auto tree = max(-Tree::Z(), r - (2 + t));

        Region<3> ra({-0.3125, -3.4375, -0.3125}, {0, -3.125, 0});
        Region<3> rb({-0.3125, -3.4375, 0}, {0, -3.125, 0.3125});
        Eigen::Vector3f target(0, -3.4375, 0);

        // Initial sanity-checking
        REQUIRE(ra.contains(target.template cast<double>()));
        REQUIRE(rb.contains(target.template cast<double>()));

        auto deck = std::make_shared<Deck>(tree);
        IntervalEvaluator eval(deck);
        PointEvaluator eval_(deck);

        float ea, eb;
        {
            auto ia = eval.evalAndPush(ra.lower.template cast<float>(),
                                       ra.upper.template cast<float>());
            CAPTURE(ia.first.lower());
            CAPTURE(ia.first.upper());
            CAPTURE(ia.second->size() / (float)deck->tape->size());
            ea = eval_.eval(target);
        }

        {
            auto ib = eval.evalAndPush(rb.lower.template cast<float>(),
                                       rb.upper.template cast<float>());
            CAPTURE(ib.first.lower());
            CAPTURE(ib.first.upper());
            CAPTURE(ib.second->size() / (float)deck->tape->size());
            eb = eval_.eval(target);
        }

        REQUIRE(ea == eb);
    }
}

TEST_CASE("IntervalEvaluator::isSafe")
{
    SECTION("Input values")
    {
        auto t = std::make_shared<Deck>(Tree::X());
        IntervalEvaluator e(t);

        e.eval({-1, 1, 0}, {1, 2, 0}, t->tape);
        REQUIRE(e.isSafe());

        e.eval({-std::numeric_limits<float>::infinity(), 1, 0},
               {1, 2, 0}, t->tape);
        REQUIRE(e.isSafe());
    }

    SECTION("Division")
    {
        auto t = std::make_shared<Deck>(Tree::X() / Tree::Y());
        IntervalEvaluator e(t);

        e.eval({-1, 1, 0}, {1, 2, 0}, t->tape);
        REQUIRE(e.isSafe());

        e.eval({-1, 0, 0}, {1, 2, 0}, t->tape);
        REQUIRE(!e.isSafe());

        e.eval({-1, -1, 0}, {1, 2, 0}, t->tape);
        REQUIRE(!e.isSafe());

        e.eval({1, -1, 0}, {2, 2, 0}, t->tape);
        REQUIRE(e.isSafe());
    }

    SECTION("Multiplication of zero and infinity")
    {
        auto t = std::make_shared<Deck>(Tree::Z() * (Tree::X() / Tree::Y()));
        IntervalEvaluator e(t);

        e.eval({-1, 1, 0}, {1, 2, 0}, t->tape);
        REQUIRE(e.isSafe());

        e.eval({-1, 0, 0}, {1, 2, 0}, t->tape);
        REQUIRE(!e.isSafe());

        e.eval({-1, -1, 1}, {1, 2, 2}, t->tape);
        REQUIRE(!e.isSafe());

        e.eval({1, -1, -1}, {2, 2, 1}, t->tape);
        REQUIRE(!e.isSafe());

        e.eval({1, -1, 1}, {2, 2, 2}, t->tape);
        REQUIRE(e.isSafe());

        e.eval({1, -1, -1}, {2, 2, 2}, t->tape);
        REQUIRE(!e.isSafe());
    }
}
