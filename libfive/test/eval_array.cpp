/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/simple_tree.hpp"
#include "libfive/eval/eval_array.hpp"

using namespace libfive;

TEST_CASE("ArrayEvaluator::eval")
{
    SECTION("X")
    {
        ArrayEvaluator e(SimpleTree::X());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        ArrayEvaluator e(SimpleTree::Y());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        ArrayEvaluator e(SimpleTree(3.14));
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Math")
    {
        auto X = SimpleTree::X();
        auto Y = SimpleTree::Y();
        ArrayEvaluator e(X*2 + Y*3);
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(8));
    }

    SECTION("Secondary variable")
    {
        auto v = SimpleTree::var();
        ArrayEvaluator e(v, {{v.id(), 3.14}});
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Constant + variable")
    {
        auto v = SimpleTree::var();
        ArrayEvaluator e(v + 1.0, {{v.id(), 3.14}});
        REQUIRE(e.value({1.0, 2.0, 3.0}) == Approx(4.14));
    }

    SECTION("Multiple variables")
    {
        // Deliberately construct out of order
        auto a = SimpleTree::var();
        auto c = SimpleTree::var();
        auto b = SimpleTree::var();

        ArrayEvaluator e(a*1 + b*2 + c*3,
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        REQUIRE(e.value({0, 0, 0}) == Approx(34));
    }

    SECTION("X + 1")
    {
        ArrayEvaluator e(SimpleTree::X() + 1);
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        ArrayEvaluator e(SimpleTree::X() + SimpleTree::Z());
        REQUIRE(e.value({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("nth-root")
    {
        ArrayEvaluator e(nth_root(SimpleTree::X(), 3));
        REQUIRE(e.value({-0.5, 0.0, 0.0}) == Approx(-0.7937));
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < libfive::Opcode::ORACLE; ++i)
        {
            auto op = (libfive::Opcode::Opcode)i;
            SimpleTree t = (Opcode::args(op) == 2
                    ? SimpleTree::binary(op, SimpleTree::X(), 5)
                    : SimpleTree::unary(op, SimpleTree::X()));
            ArrayEvaluator e(t);
            e.value({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("Behavior of mod operator")
    {
        ArrayEvaluator e(mod(SimpleTree::X(), SimpleTree::Y()));
        REQUIRE(e.value({1.1, 1.0, 0.0}) == Approx(0.1));
        REQUIRE(e.value({-1.1, 1.0, 0.0}) == Approx(0.9));
        REQUIRE(e.value({-1.1, 1.1, 0.0}) == Approx(0.0));

        REQUIRE(e.value({2.1, 2.5, 0.0}) == Approx(2.1));
        REQUIRE(e.value({2.6, 2.5, 0.0}) == Approx(0.1));

        REQUIRE(e.value({2.1, -2, 0.0}) == Approx(-1.9));
        REQUIRE(e.value({2.4, -1, 0.0}) == Approx(-0.6));

        REQUIRE(e.value({-2.1, -2, 0.0}) == Approx(-0.1));
        REQUIRE(e.value({-2.4, -1, 0.0}) == Approx(-0.4));
    }

    SECTION("SIMD consistency")
    {
        std::vector<float> pts = {0.0f, -1.0f, 1.0f, 1.000001f,
                                 -3.14159f, 19.0f, 2.718f,
                                  123123123123123123.0f};

        const auto count = 100;
        for (unsigned i=7; i < libfive::Opcode::ORACLE; ++i)
        {
            auto op = (libfive::Opcode::Opcode)i;
            CAPTURE(Opcode::toString(op));
            if (Opcode::args(op) == 1) {
                SimpleTree t = SimpleTree::unary(op, SimpleTree::X());
                ArrayEvaluator e(t);
                for (auto p : pts) {
                    CAPTURE(p);
                    for (unsigned i=0; i < count; ++i) {
                        e.set({p, 0, 0}, i);
                    }
                    auto vs = e.values(count);
                    for (unsigned i=0; i < count; ++i) {
                        CAPTURE(i);
                        if (std::isnan(vs[i])) {
                            REQUIRE(std::isnan(vs[0]));
                        } else {
                            REQUIRE(vs[i] == vs[0]);
                        }
                    }
                }
            } else if (Opcode::args(op) == 2) {
                if (op == Opcode::OP_POW || op == Opcode::OP_NTH_ROOT) {
                    continue;
                }
                SimpleTree t = SimpleTree::binary(op, SimpleTree::X(), SimpleTree::Y());
                ArrayEvaluator e(t);

                for (auto p : pts) {
                    CAPTURE(p);
                    for (auto q : pts) {
                        // Special-casing for invalid operation
                        if (op == Opcode::OP_MOD && q <= 0) { continue; }

                        CAPTURE(q);
                        for (unsigned i=0; i < count; ++i) {
                            e.set({p, q, 0}, i);
                        }
                        auto vs = e.values(count);
                        for (unsigned i=0; i < count; ++i) {
                            CAPTURE(i);
                            if (std::isnan(vs[i])) {
                                REQUIRE(std::isnan(vs[0]));
                            } else {
                                REQUIRE(vs[i] == vs[0]);
                            }
                        }
                    }
                }
            }
        }
    }
}

TEST_CASE("ArrayEvaluator::setVar")
{
    // Deliberately construct out of order
    auto a = SimpleTree::var();
    auto c = SimpleTree::var();
    auto b = SimpleTree::var();

    ArrayEvaluator e(a*1 + b*2 + c*3, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});
    REQUIRE(e.value({0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(e.value({0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(e.value({0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(e.value({0, 0, 0}) == Approx(35));
}

TEST_CASE("ArrayEvaluator::getAmbiguous")
{
    ArrayEvaluator e(min(SimpleTree::X(), -SimpleTree::X()));
    e.set({0, 0, 0}, 0);
    e.set({1, 0, 0}, 1);
    e.set({2, 0, 0}, 2);
    e.set({0, 0, 0}, 3);

    e.values(4);

    auto a = e.getAmbiguous(3);
    REQUIRE(a.count() == 1);
    REQUIRE(a(0) == true);

    auto b = e.getAmbiguous(4);
    REQUIRE(b.count() == 2);
    REQUIRE(b(0) == true);
    REQUIRE(b(3) == true);
}

TEST_CASE("ArrayEvaluator::values (returned size)")
{
    ArrayEvaluator e(min(SimpleTree::X(), -SimpleTree::X()));
    e.set({0, 0, 0}, 0);
    e.set({1, 0, 0}, 1);
    e.set({2, 0, 0}, 2);

    auto o = e.values(3);
    REQUIRE(o.cols() == 3);
}

TEST_CASE("ArrayEvaluator::valueAndPush")
{
    ArrayEvaluator e(min(SimpleTree::X(), SimpleTree::Y()));

    {
        auto h = e.valueAndPush({-1, 0, 0}); // specialize to just "X"
        REQUIRE(e.value({-2, 0, 0}, *h.second) == -2);
        REQUIRE(e.value({4, 0, 0}, *h.second) == 4);
        REQUIRE(e.value({4, 5, 0}, *h.second) == 4);
        REQUIRE(e.value({10, 5, 0}, *h.second) == 10);
    }

    {
        auto h = e.valueAndPush({0, -1, 0}); // specialize to just "Y"
        REQUIRE(e.value({-2, 0, 0}, *h.second) == 0);
        REQUIRE(e.value({4, 0, 0}, *h.second) == 0);
        REQUIRE(e.value({4, 5, 0}, *h.second) == 5);
        REQUIRE(e.value({10, 5, 0}, *h.second) == 5);
    }
}
