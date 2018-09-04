/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_jacobian.hpp"
#include "libfive/eval/deck.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("JacobianEvaluator::gradient")
{
    SECTION("constant + variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Deck>(v + 1.0);
        JacobianEvaluator e(t, {{v.id(), 3.14}});

        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(4.14));
        auto g = e.gradient({1, 2, 3});
        REQUIRE(g.size() == 1);
        REQUIRE(g.count(v.id()) == 1);
        REQUIRE(g.at(v.id()) == Approx(1));
    }

    SECTION("x * variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Deck>(Tree::X() * v);
        JacobianEvaluator e(t, {{v.id(), 1}});
        {
            auto g = e.gradient({2, 0, 0});
            REQUIRE(g.size() == 1);
            REQUIRE(g.at(v.id()) == Approx(2));
        }
        {
            auto g = e.gradient({3, 0, 0});
            REQUIRE(g.at(v.id()) == Approx(3));
        }
    }

    SECTION("Multiple variables")
    {
        // Deliberately construct out of order
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        auto t = std::make_shared<Deck>(a*1 + b*2 + c*3);
        JacobianEvaluator e(t,
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        REQUIRE(e.eval({0, 0, 0}) == Approx(34));

        auto g = e.gradient({0, 0, 0});
        REQUIRE(g.at(a.id()) == Approx(1.0f));
        REQUIRE(g.at(b.id()) == Approx(2.0f));
        REQUIRE(g.at(c.id()) == Approx(3.0f));
    }
}
