/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include <array>
#include <future>

#include "libfive/tree/tree.hpp"
#include "util/oracles.hpp"

using namespace libfive;

TEST_CASE("Joining two trees")
{
    auto t = Tree::X() + 1;

    REQUIRE(t->op == Opcode::OP_ADD);
    REQUIRE(t->lhs->op == Opcode::VAR_X);
    REQUIRE(t->rhs->op == Opcode::CONSTANT);
    REQUIRE(t->rhs->value == 1);
}

TEST_CASE("Tree::flags")
{
    auto a = Tree::var();
    REQUIRE(a->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto b = Tree::var();
    REQUIRE(b->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto c = a + b;
    REQUIRE(c->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto d = c + Tree::X();
    REQUIRE(d->flags == 0);
}

TEST_CASE("Tree::remap")
{
    SECTION("Simple")
    {
        auto x = Tree::X();
        auto y = x.remap(Tree::Y(), Tree::X(), Tree::X());
        REQUIRE(y == Tree::Y());
    }

    SECTION("Remapping to a constant")
    {
        auto x = Tree::X();
        auto t = x.remap(Tree(12), Tree::X(), Tree::X());
        REQUIRE(t == Tree(12));
    }

    SECTION("Collapsing while remapping")
    {
        auto x = Tree::X() + 5;
        auto t = x.remap(Tree(3), Tree::X(), Tree::X());
        REQUIRE(t == Tree(8));
    }
}

TEST_CASE("Tree: operator<<")
{
    SECTION("Basic")
    {
        std::stringstream ss;
        ss << (Tree::X() + 5);
        REQUIRE(ss.str() == "(+ x 5)");
    }

    SECTION("With oracle")
    {
        std::stringstream ss;
        auto o = Tree(std::unique_ptr<OracleClause>(new CubeOracleClause));
        ss << (Tree::X() + 5 + o);
        REQUIRE(ss.str() == "(+ x 5 'CubeOracle)");
    }
}

TEST_CASE("Tree::makeVarsConstant")
{
    auto v = Tree::var();
    auto w = Tree::var();
    auto a = 2 * v + 5 * w;
    auto b = a.makeVarsConstant();

    {
        std::stringstream ss;
        ss << a;
        REQUIRE(ss.str() == "(+ (* 2 var-free) (* 5 var-free))");

    }
    {
        std::stringstream ss;
        ss << b;
        REQUIRE(ss.str() == "(+ (* 2 (const-var var-free)) (* 5 (const-var var-free)))");
    }
}

TEST_CASE("Tree thread safety")
{
    // This test is only valid in debug builds, because it checks an
    // assertion in Cache::del.
    std::array<std::future<void>, 2> futures;
    for (unsigned i=0; i < futures.size(); ++i)
    {
        futures[i] = std::async(std::launch::async,
            [](){
                for (unsigned j=0; j < 100000; ++j)
                {
                    auto x = new Tree(Tree::X());
                    delete x;
                }
            });
    }

    for (auto& f : futures)
    {
        f.get();
    }

    REQUIRE(true);
}

TEST_CASE("Tree::orderedDfs")
{
    SECTION("Fully branching")
    {
        auto t = min(min(Tree::X(), Tree::Y()), min(Tree::Z(), 1.0));
        auto o = t.orderedDfs();
        auto itr = o.begin();
        REQUIRE(*itr++ == Tree::X());
        REQUIRE(*itr++ == Tree::Y());
        REQUIRE(*itr++ == min(Tree::X(), Tree::Y()));
        REQUIRE(*itr++ == Tree::Z());
        REQUIRE(*itr++ == Tree(1.0));
        REQUIRE(*itr++ == min(Tree::Z(), Tree(1.0)));
        REQUIRE(*itr++ == t);
    }
    SECTION("Self-intersecting")
    {
        auto t = min(min(Tree::X(), Tree::Y()), min(Tree::Z(), Tree::X()));
        auto o = t.orderedDfs();
        auto itr = o.begin();
        REQUIRE(*itr++ == Tree::X());
        REQUIRE(*itr++ == Tree::Y());
        REQUIRE(*itr++ == min(Tree::X(), Tree::Y()));
        REQUIRE(*itr++ == Tree::Z());
        REQUIRE(*itr++ == min(Tree::Z(), Tree::X()));
        REQUIRE(*itr++ == t);
    }
}
