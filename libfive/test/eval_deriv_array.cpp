/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <cmath>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/eval_deriv_array.hpp"

using namespace libfive;


Eigen::Vector4f deriv(DerivArrayEvaluator& d, const Eigen::Vector3f& pt)
{
    d.set(pt, 0);
    return d.derivs(1).col(0);
}

TEST_CASE("DerivArrayEvaluator::deriv")
{
    SECTION("Every operator")
    {
        for (unsigned i=0; i < libfive::Opcode::LAST_OP; ++i)
        {
            auto op = (libfive::Opcode::Opcode)i;
            if (Opcode::args(op) <= 0) {
                continue;
            }
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            DerivArrayEvaluator e(t);
            deriv(e, {0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("var + 2*X")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2.0 * Tree::X(), {{v.id(), 0}});

        auto out = deriv(e, {2, 0, 0});
        REQUIRE(out.col(0) == Eigen::Vector4f(2, 0, 0, 4));
    }
    
    SECTION("var + sinh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * sinh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {1, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(3.08616));
    }

    SECTION("var + cosh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * cosh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {1, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(2.3504));
    }

    SECTION("var + tanh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * tanh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {1, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(0.839949));
    }

    SECTION("var + asinh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * asinh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {2, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(0.894427));
    }

    SECTION("var + acosh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * acosh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {2, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(1.1547));
    }

    SECTION("var + atanh(X)")
    {
        auto v = Tree::var();
        DerivArrayEvaluator e(v + 2 * atanh(Tree::X()), {{v.id(), 0}});

        auto out = deriv(e, {0.8, 0, 0});
        REQUIRE(out.coeff(0,0) == Approx(5.55556));
    }

}

TEST_CASE("DerivArrayEvaluator::derivs")
{
    SECTION("X")
    {
        DerivArrayEvaluator e(Tree::X());
        e.set({0, 0, 0}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 0, 0));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 0, 1));
    }

    SECTION("X + Z")
    {
        DerivArrayEvaluator e(Tree::X() + Tree::Z());

        e.set({1, 1, 1}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 1, 2));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 1, 4));
    }

    SECTION("X^(1/3)")
    {
        DerivArrayEvaluator e(nth_root(Tree::X(), 3));

        e.set({0, 0, 0}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        CAPTURE(d.col(0));
        REQUIRE(std::isinf(d.col(0)(0)));
        REQUIRE(d.col(0).bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 0));

        CAPTURE(d.col(1));
        REQUIRE(d.col(1)(0) == Approx(0.33333));
        REQUIRE(d.col(1).bottomRows(3).matrix() == Eigen::Vector3f(0, 0, 1));
    }
}

TEST_CASE("DerivArrayEvaluator::getAmbiguousDerivs")
{
    DerivArrayEvaluator e(min(min(Tree::X(), Tree::Y()),
                              min(Tree::X(), 4 + 0.2 * Tree::Y())));
    e.set({0, 0, 0}, 0); // True ambiguity
    e.set({0, 1, 0}, 1); // False ambiguity, since it's doing min(X, X)
    e.set({5, 5, 0}, 2); // True ambiguity
    e.set({0, 5, 0}, 3); // False ambiguity

    e.derivs(4);

    auto a = e.getAmbiguousDerivs(4);
    REQUIRE(a.count() == 2);
    REQUIRE(a(0) == true);
    REQUIRE(a(1) == false);
    REQUIRE(a(2) == true);
    REQUIRE(a(3) == false);
}
