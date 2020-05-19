/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <sstream>

#include "catch.hpp"

#include "libfive/tree/cache.hpp"

using namespace libfive;


TEST_CASE("Cache::checkCommutative")
{
    auto t = Cache::instance();

    SECTION("No balancing")
    {
        auto a = t->operation(Opcode::OP_MIN, t->X(), t->Y());
        REQUIRE(a->rank == 1);
    }

    SECTION("LHS")
    {
        auto b = t->operation(Opcode::OP_MIN, t->X(),
                 t->operation(Opcode::OP_MIN, t->Y(),
                 t->operation(Opcode::OP_MIN, t->Z(), t->constant(1.0f))));
        REQUIRE(b->rank == 2);
    }

    SECTION("RHS")
    {
        auto b = t->operation(Opcode::OP_MIN, t->operation(Opcode::OP_MIN,
                 t->operation(Opcode::OP_MIN, t->Z(), t->constant(1.0f)),
                     t->X()), t->Y());
        REQUIRE(b->rank == 2);
    }
}

TEST_CASE("Cache::asAffine")
{
    auto t = Cache::instance();
    SECTION("X")
    {
        auto x = t->X();
        auto m = t->asAffine(x);
        REQUIRE(m.size() == 1);
        REQUIRE(m.at(x) == 1.0f);
    }

    SECTION("X + Y")
    {
        auto a = t->operation(Opcode::OP_ADD, t->X(), t->Y());
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 1.0f);
        REQUIRE(m.at(t->Y()) == 1.0f);
    }

    SECTION("2 * X + Y")
    {
        auto a = t->operation(Opcode::OP_ADD,
                t->operation(Opcode::OP_MUL, t->X(), t->constant(2)),
                t->Y(), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 2.0f);
        REQUIRE(m.at(t->Y()) == 1.0f);
    }

    SECTION("X * 2 + Y")
    {
        auto a = t->operation(Opcode::OP_ADD,
                t->operation(Opcode::OP_MUL, t->constant(2), t->X()),
                t->Y(), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 2.0f);
        REQUIRE(m.at(t->Y()) == 1.0f);
    }

    SECTION("(X * 2 + Y) * 3")
    {
        auto a = t->operation(Opcode::OP_MUL,
                t->operation(Opcode::OP_ADD,
                    t->operation(Opcode::OP_MUL, t->constant(2), t->X()),
                    t->Y(), false),
                t->constant(3), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 6.0f);
        REQUIRE(m.at(t->Y()) == 3.0f);
    }

    SECTION("(X * 2 + Y) / 3")
    {
        auto a = t->operation(Opcode::OP_DIV,
                t->operation(Opcode::OP_ADD,
                    t->operation(Opcode::OP_MUL, t->constant(2), t->X()),
                    t->Y(), false),
                t->constant(3), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == Approx(2.0f/3.0f));
        REQUIRE(m.at(t->Y()) == Approx(1.0f/3.0f));
    }

    SECTION("X + 3")
    {
        auto a = t->operation(Opcode::OP_ADD, t->constant(3), t->X());
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 1.0f);
        REQUIRE(m.at(t->constant(1.0f)) == 3.0f);
    }

    SECTION("4 + 3 * (X + 2)")
    {
        auto a = t->operation(Opcode::OP_ADD, t->constant(4),
                t->operation(Opcode::OP_MUL, t->constant(3),
                t->operation(Opcode::OP_ADD, t->X(), t->constant(2), false),
                false), false);

        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 3.0f);
        REQUIRE(m.at(t->constant(1.0f)) == 10.0f);
    }

    SECTION("X + (X - Y)")
    {
        auto a = t->operation(Opcode::OP_ADD, t->X(),
                t->operation(Opcode::OP_SUB, t->X(), t->Y(), false), false);

        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()) == 2.0f);
        REQUIRE(m.at(t->Y()) == -1.0f);
    }
}

TEST_CASE("Cache::fromAffine")
{
    auto t = Cache::instance();

    SECTION("Empty")
    {
        std::map<std::shared_ptr<Tree::Tree_>, float> empty;
        auto a_ = t->fromAffine(empty);
        REQUIRE(a_->op == Opcode::CONSTANT);
        REQUIRE(a_->value == 0.0f);
    }

    SECTION("{1: 5}")
    {
        auto a_ = t->fromAffine({{t->constant(1.0f), 5.0f}});
        REQUIRE(a_->op == Opcode::CONSTANT);
        REQUIRE(a_->value == 5.0f);
    }

    SECTION("(X * 2 + Y) / 3")
    {
        auto a = t->operation(Opcode::OP_DIV,
                t->operation(Opcode::OP_ADD,
                    t->operation(Opcode::OP_MUL, t->constant(2), t->X()),
                    t->Y(), false),
                t->constant(3), false);
        auto m = t->asAffine(a);
        auto a_ = t->fromAffine(m);

        REQUIRE(a_->op == Opcode::OP_ADD);
        REQUIRE(a_->lhs->op == Opcode::OP_MUL);
        REQUIRE(a_->lhs->lhs->op == Opcode::VAR_Y);
        REQUIRE(a_->lhs->rhs->op == Opcode::CONSTANT);
        REQUIRE(a_->lhs->rhs->value == Approx(1.0f / 3.0f));
        REQUIRE(a_->rhs->lhs->op == Opcode::VAR_X);
        REQUIRE(a_->lhs->rhs->op == Opcode::CONSTANT);
        REQUIRE(a_->rhs->rhs->value == Approx(2.0f / 3.0f));
    }

    SECTION("{X: 2, Y: -1}")
    {
        auto a = t->fromAffine({{t->X(), 2.0f}, {t->Y(), -1}});

        std::stringstream ss;
        a->print(ss);
        CAPTURE(ss.str());

        REQUIRE(a->op == Opcode::OP_SUB);
        REQUIRE(a->lhs->op == Opcode::OP_MUL);
        REQUIRE(a->lhs->lhs->op == Opcode::VAR_X);
        REQUIRE(a->lhs->rhs->op == Opcode::CONSTANT);
        REQUIRE(a->lhs->rhs->value == 2.0f);
        REQUIRE(a->rhs->op == Opcode::VAR_Y);
    }
}

TEST_CASE("Cache::checkAffine")
{
    auto t = Cache::instance();

    SECTION("X + (X - Y)")
    {
        auto a = t->operation(Opcode::OP_ADD, t->X(),
                t->operation(Opcode::OP_SUB, t->X(), t->Y()));

        std::stringstream ss;
        a->print(ss);
        CAPTURE(ss.str());

        REQUIRE(a->op == Opcode::OP_SUB);
        REQUIRE(a->lhs->op == Opcode::OP_MUL);
        REQUIRE(a->lhs->lhs->op == Opcode::VAR_X);
        REQUIRE(a->lhs->rhs->op == Opcode::CONSTANT);
        REQUIRE(a->lhs->rhs->value == 2.0f);
        REQUIRE(a->rhs->op == Opcode::VAR_Y);
    }

    SECTION("0.4 * (X - 0.1) - 0.4 * (Y - 0.6)")
    {
        auto a = t->operation(Opcode::OP_SUB,
                t->operation(Opcode::OP_MUL, t->constant(0.4),
                    t->operation(Opcode::OP_SUB, t->X(), t->constant(0.1))),
                t->operation(Opcode::OP_MUL, t->constant(0.4),
                    t->operation(Opcode::OP_SUB, t->Y(), t->constant(0.6))));

        auto m = t->asAffine(a);
        REQUIRE(m.size() == 3);
        REQUIRE(m.at(t->X()) == Approx(0.4f));
        REQUIRE(m.at(t->Y()) == Approx(-0.4f));
        REQUIRE(m.at(t->constant(1)) == Approx(0.2f));
    }
}

TEST_CASE("Cache: performance of commutative tree balancing", "[!mayfail]")
{
    auto t = Cache::instance();
    const unsigned N = 1000;

    // Build a left-biased tree without any simplification,
    // to get a baseline for how long it should take.
    auto start = std::chrono::steady_clock::now();
    auto dt = std::chrono::steady_clock::now() - start;
    auto c = t->constant(0.0f);
    for (unsigned i=0; i < N; ++i) {
        dt = std::chrono::steady_clock::now() - start;
        auto p = t->operation(Opcode::OP_ADD, t->X(), t->constant(i));
        c = t->operation(Opcode::OP_MIN, c, p, false);
    }
    REQUIRE(c->rank == N);

    // Then run the same tree-building exercise with simplifications turned
    // on, which will try to balance the binary tree.
    start = std::chrono::steady_clock::now();
    auto d = t->constant(0.0f);
    for (unsigned i=0; i < N; ++i) {
        const auto dt_ = std::chrono::steady_clock::now() - start;
        if (dt_ > dt * 15) {
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt)
                    .count());
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt_)
                    .count());
            REQUIRE(false);
        }
        auto p = t->operation(Opcode::OP_ADD, t->X(), t->constant(i));
        d = t->operation(Opcode::OP_MIN, d, p, true);
    }
    REQUIRE(d->rank < log(N) / log(2) * 2);


    // Next, build a tree that's biased in the opposite direction
    start = std::chrono::steady_clock::now();
    auto e = t->constant(0.0f);
    for (unsigned i=0; i < N; ++i) {
        const auto dt_ = std::chrono::steady_clock::now() - start;
        if (dt_ > dt * 15) {
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt)
                    .count());
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt_)
                    .count());
            REQUIRE(false);
        }
        auto p = t->operation(Opcode::OP_ADD, t->X(), t->constant(i));
        e = t->operation(Opcode::OP_MIN, p, e, true);
    }
    REQUIRE(e->rank < log(N) / log(2) * 2);

    // Just for fun, build a tree with alternating left / right insertions
    start = std::chrono::steady_clock::now();
    auto f = t->constant(0.0f);
    for (unsigned i=0; i < N; ++i) {
        const auto dt_ = std::chrono::steady_clock::now() - start;
        if (dt_ > dt * 15) {
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt)
                    .count());
            CAPTURE(std::chrono::duration_cast<std::chrono::microseconds>(dt_)
                    .count());
            REQUIRE(false);
        }
        auto p = t->operation(Opcode::OP_ADD, t->X(), t->constant(i));
        if (i & 1) {
            f = t->operation(Opcode::OP_MIN, p, f, true);
        } else {
            f = t->operation(Opcode::OP_MIN, f, p, true);
        }
    }
    REQUIRE(f->rank < log(N) / log(2) * 2);
}
