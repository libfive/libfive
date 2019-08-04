/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_interval.hpp"
#include "libfive/eval/evaluator.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/render/brep/region.hpp"

using namespace libfive;

TEST_CASE("IntervalEvaluator::eval")
{
    SECTION("Basic math")
    {
        IntervalEvaluator e(Tree::X() + 1);

        auto out = e.eval({1,1,1}, {2,2,2});

        REQUIRE(out.lower() == 2.0);
        REQUIRE(out.upper() == 3.0);
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < libfive::Opcode::ORACLE; ++i)
        {
            auto op = (libfive::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            IntervalEvaluator e(t);
            e.eval({0, 0, 0}, {1, 1, 1});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("Bounds growth")
    {
        IntervalEvaluator e(
            (Tree::X() + Tree::Y()) * (Tree::X() - Tree::Y()));

        auto o = e.eval({0, 0, 0}, {1, 1, 1});
        REQUIRE(o.lower() == -2);
        REQUIRE(o.upper() == 2);
    }
}

TEST_CASE("IntervalEvaluator::intervalAndPush")
{
    SECTION("Basic")
    {
        auto t = std::make_shared<Deck>(min(Tree::X() + 1, Tree::Y() + 1));
        IntervalEvaluator e(t);

        // Store -3 in the rhs's value
        auto o = e.eval({1, -3, 0}, {1, -3, 0});
        REQUIRE(o.lower() == -2);
        REQUIRE(o.upper() == -2);

        // Do an interval evaluation that will lead to disabling the rhs
        // Pushing should disable the rhs of min
        auto p = e.intervalAndPush({-5, 8, 0}, {-4, 9, 0});
        auto i = p.i;
        REQUIRE(i.lower() == -4);
        REQUIRE(i.upper() == -3);

        // Check to make sure that the push disabled something
        REQUIRE(p.tape->size() < t->tape->size());

        // Require that the evaluation gets 1
        o = e.eval({1, 2, 0}, {1, 2, 0});
        REQUIRE(o.lower() == 2);
        REQUIRE(o.upper() == 2);
    }

    SECTION("Multi-min trees")
    {
        auto t = min(min(Tree::Y(), Tree::X()),
                     min(Tree::X(), Tree::Y() + 3));
        auto d = std::make_shared<Deck>(t);

        IntervalEvaluator e(d);

        // Do an interval evaluation that should lead to both sides
        // picking X, then collapsing min(X, X) into just X.
        auto i = e.intervalAndPush({-5, 0, 0}, {-4, 1, 0});
        CAPTURE(d->tape->size());
        REQUIRE(i.tape->size() == 1);
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
        Evaluator eval_(deck);

        float ea, eb;
        {
            auto ia = eval.intervalAndPush(ra.lower.template cast<float>(),
                                           ra.upper.template cast<float>());
            CAPTURE(ia.i.lower());
            CAPTURE(ia.i.upper());
            CAPTURE(ia.tape->size() / (float)deck->tape->size());
            ea = eval_.value(target);
        }

        {
            auto ib = eval.intervalAndPush(rb.lower.template cast<float>(),
                                           rb.upper.template cast<float>());
            CAPTURE(ib.i.lower());
            CAPTURE(ib.i.upper());
            CAPTURE(ib.tape->size() / (float)deck->tape->size());
            eb = eval_.value(target);
        }

        REQUIRE(ea == eb);
    }

    SECTION("Fuzzing mod operator")
    {
        std::list<Interval::I> i;
        for (int a=-5; a <= 5; ++a) {
            for (int b=-5; b <= 5; ++b) {
                if (a <= b) {
                    i.push_back({float(a), float(b)});
                }
            }
        }
        auto m = mod(Tree::X(), Tree::Y());
        auto deck = std::make_shared<Deck>(m);
        auto ei = IntervalEvaluator(deck);
        auto ea = ArrayEvaluator(deck);
        for (const auto& a : i) {
            for (const auto& b : i) {
                auto r = ei.eval_({a.lower(), b.lower(), 0.0f},
                                  {a.upper(), b.upper(), 0.0f},
                                  deck->tape);
                CAPTURE(a.lower());
                CAPTURE(a.upper());
                CAPTURE(b.lower());
                CAPTURE(b.upper());
                CAPTURE(r.i.lower());
                CAPTURE(r.i.upper());
                const unsigned N = 10;
                for (unsigned c = 0; c <= N; ++c) {
                    float a_ = a.lower() * (c / float(N)) +
                               a.upper() * (1.0f - c / float(N));
                    for (unsigned d = 0; d <= N; ++d) {
                        float b_ = b.lower() * (c / float(N)) +
                                   b.upper() * (1 - c / float(N));
                        auto v = ea.value({a_, b_, 0.0f});
                        if (r.safe) {
                            REQUIRE(v >= r.i.lower());
                            REQUIRE(v <= r.i.upper());
                        }
                    }
                }
            }
        }
    }
}

TEST_CASE("IntervalEvaluator::eval_().safe")
{
    SECTION("Input values")
    {
        auto t = std::make_shared<Deck>(Tree::X());
        IntervalEvaluator e(t);

        REQUIRE(e.eval_({-1, 1, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(e.eval_({-std::numeric_limits<float>::infinity(), 1, 0},
               {1, 2, 0}, t->tape).safe);
    }

    SECTION("Division")
    {
        auto t = std::make_shared<Deck>(Tree::X() / Tree::Y());
        IntervalEvaluator e(t);

        REQUIRE(e.eval_({-1, 1, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(!e.eval_({-1, 0, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(!e.eval_({-1, -1, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(e.eval_({1, -1, 0}, {2, 2, 0}, t->tape).safe);
    }

    SECTION("Multiplication of zero and infinity")
    {
        auto t = std::make_shared<Deck>(Tree::Z() * (Tree::X() / Tree::Y()));
        IntervalEvaluator e(t);

        REQUIRE(e.eval_({-1, 1, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(!e.eval_({-1, 0, 0}, {1, 2, 0}, t->tape).safe);
        REQUIRE(!e.eval_({-1, -1, 1}, {1, 2, 2}, t->tape).safe);
        REQUIRE(!e.eval_({1, -1, -1}, {2, 2, 1}, t->tape).safe);
        REQUIRE(e.eval_({1, -1, 1}, {2, 2, 2}, t->tape).safe);
        REQUIRE(!e.eval_({1, -1, -1}, {2, 2, 2}, t->tape).safe);
    }
}
