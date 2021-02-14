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
#include "libfive/tree/data.hpp"
#include "util/oracles.hpp"

using namespace libfive;

TEST_CASE("Tree: reference counting")
{
    SECTION("Singletons")
    {
        auto t = Tree::X();
        REQUIRE(t->refcount == 2);
        {
            auto q = Tree::X();
            REQUIRE(t == q);
            REQUIRE(t->refcount == 3);
            REQUIRE(q->refcount == 3);
        }
        REQUIRE(t->refcount == 2);
    }

    SECTION("Operations and stuff")
    {
        // Using vars because they're unique
        auto a = Tree::var();
        auto b = Tree::var();
        REQUIRE(a != b);
        REQUIRE(a->refcount == 1);
        REQUIRE(b->refcount == 1);
        {
            auto c = a + b;
            REQUIRE(a->refcount == 2);
            REQUIRE(b->refcount == 2);
            REQUIRE(c->refcount == 1);
            {
                auto e = b;
                REQUIRE(b->refcount == 3);
                auto g = std::move(e);
                REQUIRE(b->refcount == 3);
                REQUIRE(e.operator->() == nullptr);
            }
        }
        REQUIRE(a->refcount == 1);
        REQUIRE(b->refcount == 1);
    }
}

TEST_CASE("Tree: basic operation")
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
        auto y = x.remap(Tree::Y(), Tree::X(), Tree::X()).flatten();
        REQUIRE(y == Tree::Y());
    }

    SECTION("Remapping to a constant")
    {
        auto x = Tree::X();
        auto t = x.remap(Tree(12), Tree::X(), Tree::X()).flatten();
        REQUIRE(t->value() == 12);
    }

    SECTION("Collapsing while remapping")
    {
        auto x = Tree::X() + 5;
        auto t = x.remap(Tree(3), Tree::X(), Tree::X()).flatten();
        CAPTURE(t);
        REQUIRE(t->value() == 8);
    }

    SECTION("Deep remapping")
    {
        auto t = Tree::X();
        auto y = Tree::Y();
        for (unsigned i=0; i < 32768; ++i) {
            t = t + y * i;
        }
        auto z = t.remap(Tree::Z(), Tree::X(), Tree::Y()).flatten();

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

        auto g = f.remap(Tree::Y(), Tree::Y(), Tree::Z()).flatten();
        REQUIRE(g.size() == 7);
    }

    SECTION("Nested") {
        auto y_plus_two = Tree::X().remap(Tree::Y() + 2, Tree::Z(), Tree::X());
        auto out = (Tree::Y() + Tree::Z()).remap(Tree::Y(), y_plus_two, y_plus_two);
        {
            std::stringstream ss;
            ss << out;
            REQUIRE(ss.str() == "(remap (+ y z) y (remap x (+ y 2) z x) (remap x (+ y 2) z x))");
        }
        {
            std::stringstream ss;
            ss << out.flatten();
            REQUIRE(ss.str() == "(+ y 2 y 2)");
        }
    }

    SECTION("Multiple") {
        auto x = Tree::X();
        auto y = Tree::Y();
        auto z = Tree::Z();
        auto c = sqrt(x*x + y*y);
        c = c.remap(x / 2, y, z);
        c = c.remap(x - -0.5, y, z - 0.25);
        std::stringstream ss;
        ss << c.flatten();
        REQUIRE(ss.str() == "(sqrt (+ (square (/ (- x -0.5) 2)) (square y)))");
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
        auto o = Tree(std::make_unique<CubeOracleClause>());
        ss << (Tree::X() + 5 + o);
        REQUIRE(ss.str() == "(+ x 5 'CubeOracle)");
    }
}

TEST_CASE("Tree thread safety")
{
    // Check reference count
    std::array<std::future<void>, 4> futures;
    for (unsigned i=0; i < futures.size(); ++i)
    {
        futures[i] = std::async(std::launch::async,
            [](){
                for (unsigned j=0; j < 100000; ++j)
                {
                    auto x = new Tree(Tree::X() + j);
                    delete x;
                }
            });
    }

    for (auto& f : futures) {
        f.get();
    }

    REQUIRE(Tree::X()->refcount == 2);
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

TEST_CASE("Tree::optimized")
{
    SECTION("(Z + 2) / (2 * (Z + 3))") {
        auto z = Tree::Z();
        auto t = (z + 2) / (2 * (z + 3));
        t = t.optimized();
        std::stringstream ss;
        ss << t;
        REQUIRE(ss.str() == "(/ (+ z 2) (+ (* z 2) 6))");
        REQUIRE(t.size() == 7);
    }

    SECTION("-3*cos(X) + Z + 2") {
        auto x = Tree::X();
        auto z = Tree::Z();
        auto t = -3 * cos(x) + z + 2;
        t = t.optimized();
        std::stringstream ss;
        ss << t;
        REQUIRE(ss.str() == "(- (+ z 2) (* (cos x) 3))");
    }

    SECTION("X*2 + Y*5 + cos(Z) + 5 * cos(Z)") {
        auto c = cos(Tree::Z());
        auto t = (Tree::X() * 2 + Tree::Y() * 5 + c + 5 * c);

        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(+ (* x 2) (* y 5) (* (cos z) 6))");
    }

    SECTION("(Z - 10) / -Z") {
        auto t = (Tree::Z() - 10) / -Tree::Z();
        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(/ (- z 10) (- z))");
    }

    SECTION("(2*X + Y) + (2*X + Y)") {
        // These trees have the same value, but don't have the same Id
        auto a = 2*Tree::X() + Tree::Y();
        auto b = 2*Tree::X() + Tree::Y();
        auto t = a + b;

        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(+ (* y 2) (* x 4))");
    }

    SECTION("(2*X + Y) * (2*X + Y)") {
        // These trees have the same value, but don't have the same Id
        auto a = 2*Tree::X() + Tree::Y();
        auto b = 2*Tree::X() + Tree::Y();
        auto t = a * b;
        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(square (+ y (* x 2)))");
    }

    SECTION("(X + 2*Y) + 3*cos(X + 2*Y)") {
        auto a = Tree::X() + (2 * Tree::Y());
        auto c = 3*cos(a);
        auto t = a + c;
        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(+ x (* y 2) (* (cos (+ x (* y 2))) 3))");
    }

    SECTION("(X + 2*Y) + 3*cos(sin(X + 2*Y + 7))") {
        auto a = Tree::X() + (2 * Tree::Y()) + 7;
        auto c = 3*cos(sin(a));
        auto t = a + c;
        std::stringstream ss;
        ss << t.optimized();
        REQUIRE(ss.str() == "(+ x (* y 2) (* (cos (sin (+ x (* y 2) 7))) 3) 7)");
    }

    SECTION("atan2(-Z / (Z - 10), -Z / (Z - 100))") {
        auto ten = Tree(10.0f);
        auto hundred = Tree(100.0f);

        auto a = -Tree::Z();
        auto b =  Tree::Z() - ten;
        auto c = -Tree::Z();
        auto d =  Tree::Z() - hundred;
        auto t = atan2(a / b, c / d);

        auto q = t.optimized();
        std::stringstream ss;
        ss << q;
        REQUIRE(ss.str() == "(atan2 (/ (- z) (- z 10)) (/ (- z) (- z 100)))");
    }

    SECTION("min(X, Y, X, Y)") {
        auto t = min(min(Tree::X(), Tree::Y()), min(Tree::X(), Tree::Y()));
        auto q = t.optimized();
        CAPTURE(q);
        REQUIRE(q.size() == 3);
    }

    SECTION("min(X + 1, Y + 1, X + 1, Y + 1)") {
        auto xa = Tree::X() + 1;
        auto xb = Tree::X() + 1;
        auto ya = Tree::Y() + 1;
        auto yb = Tree::Y() + 1;
        auto t = min(min(xa, ya), min(xb, yb));
        auto q = t.optimized();
        CAPTURE(q);
        REQUIRE(q.size() == 6);
    }

    SECTION("(- z) * (- z)") {
        auto a = -Tree::Z();

        auto t = a * a; // shared su
        auto q = t.optimized();
        std::stringstream ss;
        ss << q;
        REQUIRE(ss.str() == "(square (- z))");
    }

    SECTION("(- -0.091 (/ (- z 2.7) 0.6))") {
        auto z = Tree::Z();
        auto t = -0.091 - (z - 2.7) / 0.6;
        std::stringstream ss;
        ss << t;
        REQUIRE(ss.str() == "(- -0.091 (/ (- z 2.7) 0.6))");

        ss.str("");
        ss << t.optimized();
        REQUIRE(ss.str() == "(- 4.409 (* z 1.66667))");
    }

    SECTION("Basic") {
        auto x = Tree::X();
        auto y = Tree::Y();

        auto a = x + y;
        auto b = x + y;

        auto z = a * b;
        REQUIRE(z.size() == 5);

        auto q = z.optimized();
        REQUIRE(q.size() == 4);

        auto f = a * b + (a * 2);
        REQUIRE(f.size() == 8);

        auto g = f.optimized();
        CAPTURE(f);
        CAPTURE(g);
        REQUIRE(g.size() == 7);
    }

    SECTION("With vars") {
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        auto t = (a*1 + b*2 + c*3).optimized();
        REQUIRE(t.walk().size() == 9);
    }

    SECTION("Collapsing") {
        auto a = Tree::X() + Tree::Y();
        auto b = Tree::X() + Tree::Y();

        // a and b are different, so min(a, b) doesn't collapse
        auto c = min(a, b);
        REQUIRE(c.walk().size() == 5);

        // However, post-collapse, this should hit the min(x, x) = x identity
        auto d = c.optimized();
        REQUIRE(d.walk().size() == 3);
    }

    SECTION("Deduplication of constants") {
        auto ca = Tree(3.14) * Tree::X();
        auto cb = Tree(3.14) * Tree::Y();
        auto p = ca + cb;
        CAPTURE(p);
        CAPTURE(p.optimized());
        REQUIRE(p.optimized().size() == 5); // --> (* (+ y x) 3.14)

        auto cc = Tree(4) * Tree::Y();
        auto q = ca + cc;
        REQUIRE(q.optimized().size() == 7);
    }

    SECTION("Handling of NaN") {
        auto ca = sin(Tree(NAN) * Tree::X());
        CAPTURE(ca);
        REQUIRE(ca.size() == 4);
        CAPTURE(ca.optimized());
        REQUIRE(ca.optimized().size() == 4);
    }

    SECTION("Deduplication of NaN") {
        auto cx = Tree(1);
        auto ca = sin(Tree(NAN) * Tree::X());
        auto cb = Tree(std::nanf(""));
        auto cy = Tree(2);

        CAPTURE((ca + cb).optimized());
        REQUIRE((ca + cb).optimized().size() == 5);

        CAPTURE((ca + cx).optimized());
        REQUIRE((ca + cx).optimized().size() == 6);

        CAPTURE((ca + cy).optimized());
        REQUIRE((ca + cy).optimized().size() == 6);
    }

    SECTION("CSE of commutative operations") {
        auto a = min(2 * Tree::X(), 3 * Tree::Y() + Tree::Z());
        auto b = min(3 * Tree::Y() + Tree::Z(), 2 * Tree::X());
        auto c = 1 + a * (3 * b);
        auto d = c.optimized();
        CAPTURE(d);
        REQUIRE(d.size() == 13);
    }
}

TEST_CASE("Tree::flags")
{
    auto t = Tree::X();
    REQUIRE(t->flags == TreeData::TREE_FLAG_HAS_XYZ);

    t = Tree::X() + Tree::Y();
    REQUIRE(t->flags == TreeData::TREE_FLAG_HAS_XYZ);

    t = (Tree::X() + Tree::Y()).remap(Tree::Y(), Tree::Z(), Tree::X());
    REQUIRE(t->flags == (TreeData::TREE_FLAG_HAS_XYZ |
                         TreeData::TREE_FLAG_HAS_REMAP));

    t = t.flatten();
    REQUIRE(t->flags == TreeData::TREE_FLAG_HAS_XYZ);

    t = Tree(std::make_unique<CubeOracleClause>());
    REQUIRE(t->flags == TreeData::TREE_FLAG_HAS_ORACLE);
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

TEST_CASE("Tree::eq") {
    SECTION("X") {
        // These are also equal by pointer equality, since they're singletons
        REQUIRE(Tree::X().eq(Tree::X()));
    }

#define REQUIRE_EQ(t) SECTION(#t) {\
        auto a = (t); \
        auto b = (t); \
        CAPTURE(a); \
        CAPTURE(b); \
        REQUIRE(a != b); \
        REQUIRE(a.eq(b)); \
    } while (0)

    REQUIRE_EQ(Tree::X() + 2);
    REQUIRE_EQ((Tree::X() + 2 * Tree::Y()).remap(
                Tree::Y() + 1, cos(Tree::Z()), Tree::Y() * 2));
    REQUIRE_EQ(((Tree::X() + 1.3) / Tree::Y()).optimized());
#undef REQUIRE_EQ
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

TEST_CASE("Tree::apply")
{
    auto a = Tree::var();
    auto b = (a + 1.3) / Tree::X();

    {
        std::stringstream out;
        out << b;
        REQUIRE(out.str() == "(/ (+ var-free 1.3) x)");
    }

    {
        auto c = b.apply(a, Tree::X() + Tree::Y())
            .remap(Tree::X(), 2*Tree::Z(), Tree::Y());
        std::stringstream out;
        out << c.optimized();
        REQUIRE(out.str() == "(/ (+ x 1.3 (* z 2)) x)");
    }
}

TEST_CASE("Tree::cooptimize")
{
    auto a = Tree::var();
    auto b = ((a + 1.3) / Tree::X()).optimized();
    auto c = ((a + 1.3) / Tree::X()).optimized();
    REQUIRE(a.id() != b.id());

    std::unordered_map<TreeDataKey, Tree> canonical;
    b = b.cooptimize(canonical);
    c = c.cooptimize(canonical);
    REQUIRE(b == c);
}
