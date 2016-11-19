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

#include "ao/kernel/tree/cache.hpp"

TEST_CASE("Constructing a simple shape")
{
    Cache::reset();
    auto t = Cache::instance();

    Cache::Id out = t->operation(Opcode::ADD, t->X(),
            t->constant(1));
    REQUIRE(out == 3);
}

TEST_CASE("Deduplication of variables")
{
    Cache::reset();
    auto t = Cache::instance();

    Cache::Id xa = t->X();
    Cache::Id xb = t->X();
    REQUIRE(xa == xb);

    Cache::Id ya = t->Y();
    REQUIRE(xa != ya);
}

TEST_CASE("Deduplication of constants")
{
    Cache::reset();
    auto t = Cache::instance();

    Cache::Id ca = t->constant(3.14);
    Cache::Id cb = t->constant(3.14);
    REQUIRE(ca == cb);

    Cache::Id cc = t->constant(4);
    REQUIRE(ca != cc);
}

TEST_CASE("Deduplication of operations")
{
    Cache::reset();
    auto t = Cache::instance();

    Cache::Id oa = t->operation(Opcode::ADD, t->X(), t->constant(1));
    Cache::Id ob = t->operation(Opcode::ADD, t->X(), t->constant(1));
    REQUIRE(oa == ob);

    Cache::Id oc = t->operation(Opcode::ADD, t->X(), t->constant(2));
    REQUIRE(oa != oc);
}

TEST_CASE("Found flag propagation")
{
    Cache::reset();
    auto t = Cache::instance();

    Cache::Id oa = t->operation(Opcode::ADD, t->X(), t->constant(1));
    Cache::Id ob = t->operation(Opcode::MUL, t->Y(), t->constant(1));

    auto f = t->findConnected(oa);

    REQUIRE(f.count(oa));
    REQUIRE(f.count(t->X()));
    REQUIRE(f.count(t->constant(1)));

    REQUIRE(!f.count(ob));
    REQUIRE(!f.count(t->Y()));
}

TEST_CASE("Automatic expression pruning")
{
    Cache::reset();
    auto t = Cache::instance();

    SECTION("Addition")
    {
        Cache::Id oa = t->operation(Opcode::ADD, t->X(), t->constant(0));
        REQUIRE(oa == t->X());

        Cache::Id ob = t->operation(Opcode::ADD, t->constant(0), t->X());
        REQUIRE(ob == t->X());
    }

    SECTION("Subtraction")
    {
        Cache::Id oa = t->operation(Opcode::SUB, t->X(), t->constant(0));
        REQUIRE(oa == t->X());

        Cache::Id ob = t->operation(Opcode::SUB, t->constant(0), t->X());
        REQUIRE(t->opcode(ob) == Opcode::NEG);
        REQUIRE(t->lhs(ob) == t->X());
    }

    SECTION("Multiplication")
    {
        Cache::Id oa = t->operation(Opcode::MUL, t->X(), t->constant(1));
        REQUIRE(oa == t->X());

        Cache::Id ob = t->operation(Opcode::MUL, t->constant(1), t->X());
        REQUIRE(ob == t->X());

        Cache::Id oc = t->operation(Opcode::MUL, t->X(), t->constant(0));
        REQUIRE(t->opcode(oc) == Opcode::CONST);
        REQUIRE(t->value(oc) == 0);

        Cache::Id od = t->operation(Opcode::MUL, t->constant(0), t->X());
        REQUIRE(t->opcode(od) == Opcode::CONST);
        REQUIRE(t->value(od) == 0);
    }
}

TEST_CASE("Collapsing affine trees")
{
    Cache::reset();
    auto t = Cache::instance();

    auto a_affine = t->affine(1.0, 0.0, 0.0, 3.0);
    /*
     * The tree should be of the form
     *              AFFINE
     *             /      \
     *          ADD         ADD
     *        /    \      /      \
     *      MUL    MUL   MUL     3
     *    /   \   /  \  /   \
     *   X    1  Y   0  Z   0
     *
     *  (and all of the zeros should point to the same token)
     */
    REQUIRE(a_affine == 12);

    auto collapsed = t->collapse(a_affine);
    REQUIRE(collapsed == 13);
}

TEST_CASE("Cache::findConnected")
{
    Cache::reset();
    auto t = Cache::instance();
    auto connected = t->findConnected(t->X());
    REQUIRE(connected.size() == 1);
}
