/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017-2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <cmath>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_deriv.hpp"

using namespace Kernel;

TEST_CASE("DerivEvaluator::deriv")
{
    SECTION("Every operator")
    {
        for (unsigned i=7; i < Kernel::Opcode::ORACLE; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            DerivEvaluator e(t);
            e.deriv({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("var + 2*X")
    {
        auto v = Tree::var();
        DerivEvaluator e(v + 2 * Tree::X(), {{v.id(), 0}});

        auto out = e.deriv({2, 0, 0});
        REQUIRE(out == Eigen::Vector4f(2, 0, 0, 4));
    }

    SECTION("X^(1/3)")
    {
        DerivEvaluator e(nth_root(Tree::X(), 3));

        {
            auto out = e.deriv({0, 0, 0});
            CAPTURE(out);
            REQUIRE(std::isinf(out(0)));
            REQUIRE(out.bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 0));
        }

        {
            auto out = e.deriv({1, 2, 3});
            CAPTURE(out);
            REQUIRE(out(0) == Approx(0.33333));
            REQUIRE(out.bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 1));
        }
    }
}
