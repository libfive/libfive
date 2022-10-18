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

TEST_CASE("Deduplication of variables")
{
    auto t = Cache::instance();

    auto xa = t->X();
    auto xb = t->X();
    REQUIRE(xa == xb);

    auto ya = t->Y();
    REQUIRE(xa != ya);
}

TEST_CASE("Deduplication of constants")
{
    auto t = Cache::instance();

    auto ca = t->constant(3.14);
    auto cb = t->constant(3.14);
    REQUIRE(ca == cb);

    auto cc = t->constant(4);
    REQUIRE(ca != cc);
}

TEST_CASE("Deduplication of NaN")
{
    auto t = Cache::instance();

    auto cx = t->constant(1);
    auto ca = t->constant(NAN);
    auto cb = t->constant(std::nanf(""));
    auto cy = t->constant(2);

    REQUIRE(ca == cb);
    REQUIRE(ca != cx);
    REQUIRE(ca != cy);
}

TEST_CASE("Deduplication of operations")
{
    auto t = Cache::instance();

    auto oa = t->operation(Opcode::OP_ADD, t->X(), t->constant(1));
    auto ob = t->operation(Opcode::OP_ADD, t->X(), t->constant(1));
    REQUIRE(oa == ob);

    auto oc = t->operation(Opcode::OP_ADD, t->X(), t->constant(2));
    REQUIRE(oa != oc);
}

TEST_CASE("Cache::checkIdentity")
{
    auto t = Cache::instance();

    SECTION("Addition")
    {
        auto oa = t->operation(Opcode::OP_ADD, t->X(), t->constant(0));
        REQUIRE(oa == t->X());

        auto ob = t->operation(Opcode::OP_ADD, t->constant(0), t->X());
        REQUIRE(ob == t->X());

        auto oc = t->operation(Opcode::OP_ADD, t->Y(),
                t->operation(Opcode::OP_NEG, t->X()));
        REQUIRE(oc == t->operation(Opcode::OP_SUB, t->Y(), t->X()));
    }

    SECTION("Subtraction")
    {
        auto oa = t->operation(Opcode::OP_SUB, t->X(), t->constant(0));
        REQUIRE(oa == t->X());

        auto ob = t->operation(Opcode::OP_SUB, t->constant(0), t->X());
        REQUIRE(ob->op == Opcode::OP_NEG);
        REQUIRE(ob->lhs == t->X());
    }

    SECTION("Multiplication")
    {
        auto oa = t->operation(Opcode::OP_MUL, t->X(), t->constant(1));
        REQUIRE(oa == t->X());

        auto ob = t->operation(Opcode::OP_MUL, t->constant(1), t->X());
        REQUIRE(ob == t->X());

        auto oc = t->operation(Opcode::OP_MUL, t->X(), t->constant(0));
        REQUIRE(oc->op == Opcode::CONSTANT);
        REQUIRE(oc->value == 0);

        auto od = t->operation(Opcode::OP_MUL, t->constant(0), t->X());
        REQUIRE(od->op == Opcode::CONSTANT);
        REQUIRE(od->value == 0);

        auto oe = t->operation(Opcode::OP_MUL, t->constant(-1), t->X());
        REQUIRE(oe->op == Opcode::OP_NEG);
        REQUIRE(oe->lhs == t->X());

        auto of = t->operation(Opcode::OP_MUL, t->X(), t->constant(-1));
        REQUIRE(of->op == Opcode::OP_NEG);
        REQUIRE(of->lhs == t->X());
    }

    SECTION("Exponentiation")
    {
        auto oa = t->operation(Opcode::OP_POW, t->X(), t->constant(1));
        REQUIRE(oa == t->X());

        auto ob = t->operation(Opcode::OP_NTH_ROOT, t->X(), t->constant(1));
        REQUIRE(ob == t->X());
    }

    SECTION("Double negative")
    {
        auto oa = t->operation(Opcode::OP_MUL, t->X(), t->constant(-1));
        REQUIRE(oa->op == Opcode::OP_NEG);
        REQUIRE(oa->lhs == t->X());

        auto ob = t->operation(Opcode::OP_MUL, oa, t->constant(-1));
        std::stringstream ss;
        ob->print(ss);
        CAPTURE(ss.str());
        REQUIRE(ob == t->X());
    }

    SECTION("Idempotent unary operators")
    {
        auto oa = t->operation(Opcode::OP_ABS, t->X());
        REQUIRE(oa->op == Opcode::OP_ABS);
        REQUIRE(oa->lhs == t->X());

        auto ob = t->operation(Opcode::OP_ABS, oa);
        REQUIRE(ob == oa);
    }

    SECTION("Min/max")
    {
        auto oa = t->operation(Opcode::OP_MIN, t->X(), t->X());
        REQUIRE(oa->op == Opcode::VAR_X);

        auto ia = t->operation(Opcode::OP_MIN, t->X(), t->Y());
        REQUIRE(ia->op != Opcode::VAR_X);

        auto ob = t->operation(Opcode::OP_MAX, t->X(), t->X());
        REQUIRE(ob->op == Opcode::VAR_X);

        auto ib = t->operation(Opcode::OP_MAX, t->X(), t->Y());
        REQUIRE(ib->op != Opcode::VAR_Y);
    }
}

TEST_CASE("Collapsing constants")
{
    auto t = Cache::instance();

    auto a = t->operation(Opcode::OP_ADD, t->constant(4), t->constant(3));
    REQUIRE(a->op == Opcode::CONSTANT);
    REQUIRE(a->value == 7);

    auto b = t->operation(Opcode::OP_NEG, t->constant(4));
    REQUIRE(b->op == Opcode::CONSTANT);
    REQUIRE(b->value == -4);
}

TEST_CASE("Cache::var")
{
    auto t = Cache::instance();
    auto a = t->var();
    auto b = t->var();
    REQUIRE(a != b);
}

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
        REQUIRE(m.at(x).first == 1.0f);
    }

    SECTION("X + Y")
    {
        auto a = t->operation(Opcode::OP_ADD, t->X(), t->Y());
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 1.0f);
        REQUIRE(m.at(t->Y()).first == 1.0f);
    }

    SECTION("2 * X + Y")
    {
        auto a = t->operation(Opcode::OP_ADD,
                t->operation(Opcode::OP_MUL, t->X(), t->constant(2)),
                t->Y(), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 2.0f);
        REQUIRE(m.at(t->Y()).first == 1.0f);
    }

    SECTION("X * 2 + Y")
    {
        auto a = t->operation(Opcode::OP_ADD,
                t->operation(Opcode::OP_MUL, t->constant(2), t->X()),
                t->Y(), false);
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 2.0f);
        REQUIRE(m.at(t->Y()).first == 1.0f);
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
        REQUIRE(m.at(t->X()).first == 6.0f);
        REQUIRE(m.at(t->Y()).first == 3.0f);
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
        REQUIRE(m.at(t->X()).first == Approx(2.0f/3.0f));
        REQUIRE(m.at(t->Y()).first == Approx(1.0f/3.0f));
    }

    SECTION("X + 3")
    {
        auto a = t->operation(Opcode::OP_ADD, t->constant(3), t->X());
        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 1.0f);
        REQUIRE(m.at(t->constant(1.0f)).first == 3.0f);
    }

    SECTION("4 + 3 * (X + 2)")
    {
        auto a = t->operation(Opcode::OP_ADD, t->constant(4),
                t->operation(Opcode::OP_MUL, t->constant(3),
                t->operation(Opcode::OP_ADD, t->X(), t->constant(2), false),
                false), false);

        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 3.0f);
        REQUIRE(m.at(t->constant(1.0f)).first == 10.0f);
    }

    SECTION("X + (X - Y)")
    {
        auto a = t->operation(Opcode::OP_ADD, t->X(),
                t->operation(Opcode::OP_SUB, t->X(), t->Y(), false), false);

        auto m = t->asAffine(a);
        REQUIRE(m.size() == 2);
        REQUIRE(m.at(t->X()).first == 2.0f);
        REQUIRE(m.at(t->Y()).first == -1.0f);
    }
}

TEST_CASE("Cache::fromAffine")
{
    auto t = Cache::instance();

    SECTION("Empty")
    {
        std::map<std::shared_ptr<Tree::Tree_>, std::pair<float, int>> empty;
        auto a_ = t->fromAffine(empty);
        REQUIRE(a_->op == Opcode::CONSTANT);
        REQUIRE(a_->value == 0.0f);
    }

    SECTION("{1: 5}")
    {
        auto a_ = t->fromAffine({ {t->constant(1.0f), {5.0f, 0}} });
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
        auto a = t->fromAffine({ {t->X(), {2.0f, 0}}, {t->Y(), {-1, 1}} });

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
        REQUIRE(m.at(t->X()).first == Approx(0.4f));
        REQUIRE(m.at(t->Y()).first == Approx(-0.4f));
        REQUIRE(m.at(t->constant(1)).first == Approx(0.2f));
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
