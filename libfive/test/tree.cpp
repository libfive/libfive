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

    REQUIRE(t->op() == Opcode::OP_ADD);
    REQUIRE(t->lhs()->op() == Opcode::VAR_X);
    REQUIRE(t->rhs()->op() == Opcode::CONSTANT);
    REQUIRE(t->rhs()->value() == 1);
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
        REQUIRE(t->value() == 12);
    }

    SECTION("Collapsing while remapping")
    {
        auto x = Tree::X() + 5;
        auto t = x.remap(Tree(3), Tree::X(), Tree::X());
        REQUIRE(t->value() == 8);
    }

    SECTION("Deep remapping")
    {
        auto t = Tree::X();
        auto y = Tree::Y();
        for (unsigned i=0; i < 32768; ++i) {
            t = t + y * i;
        }
        auto z = t.remap(Tree::Z(), Tree::X(), Tree::Y());

        // Make sure the original hasn't changed
        REQUIRE(t->op() == Opcode::OP_ADD);
        REQUIRE(t->lhs()->op() == Opcode::OP_ADD);
        REQUIRE(t->rhs()->op() == Opcode::OP_MUL);
        REQUIRE(t->rhs()->lhs()->op() == Opcode::VAR_Y);
        REQUIRE(t->rhs()->rhs()->op() == Opcode::CONSTANT);
        REQUIRE(t->lhs()->rhs()->lhs()->op() == Opcode::VAR_Y);
        REQUIRE(t->lhs()->rhs()->rhs()->op() == Opcode::CONSTANT);

        // Check that the remapping went through
        REQUIRE(z->op() == Opcode::OP_ADD);
        REQUIRE(z->lhs()->op() == Opcode::OP_ADD);
        REQUIRE(z->rhs()->op() == Opcode::OP_MUL);
        REQUIRE(z->rhs()->lhs()->op() == Opcode::VAR_X);
        REQUIRE(z->rhs()->rhs()->op() == Opcode::CONSTANT);
        REQUIRE(z->lhs()->rhs()->lhs()->op() == Opcode::VAR_X);
        REQUIRE(z->lhs()->rhs()->rhs()->op() == Opcode::CONSTANT);

        auto f = Tree::X();
        f = f * 2 + f * 3 + f;
        REQUIRE(f.size() == 7);

        auto g = f.remap(Tree::Y(), Tree::Y(), Tree::Z());
        REQUIRE(g.size() == 7);
    }
}

TEST_CASE("Tree::size()")
{
    auto x = Tree::X();
    REQUIRE(x.size() == 1);

    auto y = Tree::Y();
    REQUIRE(y.size() == 1);

    auto t = x + y;
    REQUIRE(t.size() == 3);

    auto z = x + x;
    REQUIRE(z.size() == 2);

    auto q = x + Tree::X();
    REQUIRE(q.size() == 2); // X is a singleton
}

TEST_CASE("Tree::unique()")
{
    SECTION("Basic") {
        auto x = Tree::X();
        auto y = Tree::Y();

        auto a = x + y;
        auto b = x + y;

        auto z = a * b;
        REQUIRE(z.size() == 5);

        auto q = z.unique();
        REQUIRE(q.size() == 4);

        auto f = a * b + (a * 2);
        REQUIRE(f.size() == 8);

        auto g = f.unique();
        REQUIRE(g.size() == 7);
    }

    SECTION("With vars") {
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        auto t = (a*1 + b*2 + c*3).unique();
        REQUIRE(t.walk().size() == 9);
    }

    SECTION("Collapsing") {
        auto a = Tree::X() + Tree::Y();
        auto b = Tree::X() + Tree::Y();

        // a and b are different, so min(a, b) doesn't collapse
        auto c = min(a, b);
        REQUIRE(c.walk().size() == 5);

        // However, post-collapse, this should hit the min(x, x) = x identity
        auto d = c.unique();
        REQUIRE(d.walk().size() == 3);
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
        auto oracle = std::make_unique<CubeOracleClause>();
        auto o = Tree(std::move(oracle));
        ss << (Tree::X() + 5 + o);
        REQUIRE(ss.str() == "(+ x 5 'CubeOracle)");
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

TEST_CASE("Tree::walk")
{
    SECTION("Fully branching")
    {
        auto t = min(min(Tree::X(), Tree::Y()), min(Tree::Z(), 1.0));
        auto o = t.walk();
        auto itr = o.begin();
        REQUIRE(*itr++ == Tree::X().get());
        REQUIRE(*itr++ == Tree::Y().get());
        {
            auto m = std::get_if<TreeBinaryOp>(*itr++);
            REQUIRE(m != nullptr);
            REQUIRE(m->op == Opcode::OP_MIN);
            REQUIRE(m->lhs == Tree::X());
            REQUIRE(m->rhs == Tree::Y());
        }
        REQUIRE(*itr++ == Tree::Z().get());
        {
            auto m = std::get_if<TreeConstant>(*itr++);
            REQUIRE(m != nullptr);
            REQUIRE(m->value == 1.0);
        }
        {
            auto m = std::get_if<TreeBinaryOp>(*itr++);
            REQUIRE(m != nullptr);
            REQUIRE(m->op == Opcode::OP_MIN);
            REQUIRE(m->lhs == Tree::Z());
        }
    }
    SECTION("Self-intersecting")
    {
        auto t = min(min(Tree::X(), Tree::Y()), min(Tree::Z(), Tree::X()));
        auto o = t.walk();
        auto itr = o.begin();
        REQUIRE(*itr++ == Tree::X().get());
        REQUIRE(*itr++ == Tree::Y().get());
        itr++;
        //REQUIRE(*itr++ == min(Tree::X(), Tree::Y()));
        REQUIRE(*itr++ == Tree::Z().get());
        //REQUIRE(*itr++ == min(Tree::Z(), Tree::X()));
        itr++;
        //REQUIRE(*itr++ == t);
    }
}

TEST_CASE("Tree::with_const_vars") {
    auto v = Tree::var();
    auto w = Tree::var();
    auto a = 2 * v + 5 * w;
    auto b = a.with_const_vars();

    {
        std::stringstream ss;
        ss << a;
        REQUIRE(ss.str() == "(+ (* 2 var-free) (* 5 var-free))");

    }
    {
        std::stringstream ss;
        ss << b;
        REQUIRE(ss.str() == "(const-var (+ (* 2 var-free) (* 5 var-free)))");
    }
}

TEST_CASE("Tree::explore_affine") {
    SECTION("Basic") {
        auto c = cos(Tree::Z());
        auto t = Tree::X() * 2 + Tree::Y() * 5 + c + 5 * c;
        Tree::AffineMap root_map;
        t.explore_affine(root_map, nullptr, 1);
        REQUIRE(root_map.size() == 1);
        auto itr = root_map.find(t.id());
        REQUIRE(itr != root_map.end());

        std::map<Tree::Id, float> map(itr->second.begin(), itr->second.end());
        REQUIRE(map.size() == 3);
        REQUIRE(map.at(Tree::X().id()) == 2);
        REQUIRE(map.at(Tree::Y().id()) == 5);
        REQUIRE(map.at(c.id()) == 6);
    }

    SECTION("(X + Y) + (X + Y)") {
        // These trees have the same value, but don't have the same Id
        auto a = Tree::X() + Tree::Y();
        auto b = Tree::X() + Tree::Y();
        Tree::AffineMap root_map;
        auto t = a + b;
        t.explore_affine(root_map, nullptr, 1);
        REQUIRE(root_map.size() == 1);
        auto itr = root_map.find(t.id());
        REQUIRE(itr != root_map.end());

        std::map<Tree::Id, float> map(itr->second.begin(), itr->second.end());
        REQUIRE(map.size() == 2);
        REQUIRE(map.at(Tree::X().id()) == 2);
        REQUIRE(map.at(Tree::Y().id()) == 2);
    }

    SECTION("(X + Y) * (X + Y)") {
        // These trees have the same value, but don't have the same Id
        auto a = Tree::X() + Tree::Y();
        auto b = Tree::X() + Tree::Y();
        Tree::AffineMap root_map;
        auto t = a * b;
        t.explore_affine(root_map, nullptr, 1);
        REQUIRE(root_map.size() == 2);
        {
            auto itr = root_map.find(a.id());
            REQUIRE(itr != root_map.end());

            std::map<Tree::Id, float> map(itr->second.begin(), itr->second.end());
            REQUIRE(map.size() == 2);
            REQUIRE(map.at(Tree::X().id()) == 1);
            REQUIRE(map.at(Tree::Y().id()) == 1);
        }
        {
            auto itr = root_map.find(b.id());
            REQUIRE(itr != root_map.end());

            std::map<Tree::Id, float> map(itr->second.begin(), itr->second.end());
            REQUIRE(map.size() == 2);
            REQUIRE(map.at(Tree::X().id()) == 1);
            REQUIRE(map.at(Tree::Y().id()) == 1);
        }
    }
}

TEST_CASE("Tree::serialize")
{
    SECTION("Basic")
    {
        auto a = min(Tree::X(), Tree::Y());
        std::stringstream out;
        a.serialize(out);

        std::string expected =
            {'T', '"', '"', '"', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }

    SECTION("With local references")
    {
        auto a = min(Tree::X(), Tree::Y() + Tree::X());
        std::stringstream out;
        a.serialize(out);
        std::string expected =
            {'T', '"', '"', '"', '"', Opcode::VAR_Y, Opcode::VAR_X, Opcode::OP_ADD, 1, 0, 0, 0, 0, 0, 0, 0, Opcode::OP_MIN, 2, 0, 0, 0, 1, 0, 0, 0, (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }
}

TEST_CASE("Tree::deserialize")
{
    SECTION("Simple")
    {
        std::stringstream out;
        min(Tree::X(), Tree::Y()).serialize(out);

        std::stringstream in(out.str());
        auto a = Tree::deserialize(in);

        REQUIRE(a.id() != nullptr);
        REQUIRE(a->op() == Opcode::OP_MIN);
        REQUIRE(a->lhs()->op() == Opcode::VAR_X);
        REQUIRE(a->rhs()->op() == Opcode::VAR_Y);
    }

    SECTION("With constant")
    {
        std::stringstream out;
        min(Tree::X(), Tree(2.5f)).serialize(out);

        std::stringstream in(out.str());
        auto a = Tree::deserialize(in);

        REQUIRE(a.id() != nullptr);
        REQUIRE(a->op() == Opcode::OP_MIN);
        REQUIRE(a->lhs()->op() == Opcode::VAR_X);
        REQUIRE(a->rhs()->op() == Opcode::CONSTANT);
        REQUIRE(a->rhs()->value() == 2.5f);
    }

    SECTION("With variable")
    {
        std::stringstream out;
        min(Tree::X(), Tree::var()).serialize(out);

        std::stringstream in(out.str());
        auto a = Tree::deserialize(in);

        REQUIRE(a.id() != nullptr);
        REQUIRE(a->op() == Opcode::OP_MIN);
        REQUIRE(a->lhs()->op() == Opcode::VAR_X);
        REQUIRE(a->rhs()->op() == Opcode::VAR_FREE);
    }
}


////////////////////////////////////////////////////////////////////////////////
// Taken from old test/cache.cpp

TEST_CASE("Tree: deduplication of XYZ")
{
    auto xa = Tree::X();
    auto xb = Tree::X();
    REQUIRE(xa == xb);

    auto ya = Tree::Y();
    REQUIRE(xa != ya);
}

TEST_CASE("Tree::unique: deduplication of constants")
{
    auto ca = Tree(3.14) * Tree::X();
    auto cb = Tree(3.14) * Tree::Y();
    auto p = ca + cb;
    REQUIRE(p.unique().size() == 6);

    auto cc = Tree(4) * Tree::Y();
    auto q = ca + cc;
    REQUIRE(q.unique().size() == 7);
}

TEST_CASE("Tree::unique: deduplication of NAN")
{
    auto cx = Tree(1);
    auto ca = Tree(NAN) * Tree::X();
    auto cb = Tree(std::nanf(""));
    auto cy = Tree(2);

    REQUIRE((ca + cb).unique().size() == 4);
    REQUIRE((ca + cx).unique().size() == 5);
    REQUIRE((ca + cy).unique().size() == 5);
}

TEST_CASE("Tree: identity operations")
{
    SECTION("Addition")
    {
        auto oa = Tree::X() + 0;
        REQUIRE(oa == Tree::X());

        auto ob = 0 + Tree::X();
        REQUIRE(ob == Tree::X());


        auto oc = Tree::X() + (-Tree::Y());
        REQUIRE(oc->op() == Opcode::OP_SUB);
        REQUIRE(oc->lhs() == Tree::X());
        REQUIRE(oc->rhs() == Tree::Y());
    }

    SECTION("Subtraction")
    {
        auto oa = Tree::X() - 0;
        REQUIRE(oa == Tree::X());

        auto ob = 0 - Tree::X();
        REQUIRE(ob->op() == Opcode::OP_NEG);
        REQUIRE(ob->lhs() == Tree::X());
    }

    SECTION("Multiplication")
    {
        auto oa = Tree::X() * 1;
        REQUIRE(oa == Tree::X());

        auto ob = 1 * Tree::X();
        REQUIRE(ob == Tree::X());

        auto oc = Tree::X() * 0;
        REQUIRE(oc->op() == Opcode::CONSTANT);
        REQUIRE(oc->value() == 0);

        auto od = 0 * Tree::X();
        REQUIRE(od->op() == Opcode::CONSTANT);
        REQUIRE(od->value() == 0);

        auto oe = -1 * Tree::X();
        REQUIRE(oe->op() == Opcode::OP_NEG);
        REQUIRE(oe->lhs() == Tree::X());

        auto of = Tree::X() * -1;
        REQUIRE(of->op() == Opcode::OP_NEG);
        REQUIRE(of->lhs() == Tree::X());
    }

    SECTION("Exponentiation")
    {
        auto oa = pow(Tree::X(), 1);
        REQUIRE(oa == Tree::X());

        auto ob = nth_root(Tree::X(), 1);
        REQUIRE(ob == Tree::X());
    }

    SECTION("Double negative")
    {
        auto oa = Tree::X() * -1;
        REQUIRE(oa->op() == Opcode::OP_NEG);
        REQUIRE(oa->lhs() == Tree::X());

        auto ob = oa * -1;
        REQUIRE(ob == Tree::X());
    }

    SECTION("Idempotent unary operators")
    {
        auto oa = abs(Tree::X());
        REQUIRE(oa->op() == Opcode::OP_ABS);
        REQUIRE(oa->lhs() == Tree::X());

        auto ob = abs(oa);
        REQUIRE(ob == oa);
    }

    SECTION("Min/max")
    {
        auto oa = min(Tree::X(), Tree::X());
        REQUIRE(oa == Tree::X());

        auto ia = min(Tree::X(), Tree::Y());
        REQUIRE(ia->op() == Opcode::OP_MIN);

        auto ob = max(Tree::X(), Tree::X());
        REQUIRE(ob == Tree::X());

        auto ib = max(Tree::X(), Tree::Y());
        REQUIRE(ib->op() == Opcode::OP_MAX);
    }
}

TEST_CASE("Tree: collapsing constants")
{
    auto a = Tree(4) + Tree(3);
    REQUIRE(a->op() == Opcode::CONSTANT);
    REQUIRE(a->value() == 7);

    auto b = -Tree(4);
    REQUIRE(b->op() == Opcode::CONSTANT);
    REQUIRE(b->value() == -4);
}

TEST_CASE("Tree::var")
{
    auto a = Tree::var();
    auto b = Tree::var();
    REQUIRE(a != b);
}
