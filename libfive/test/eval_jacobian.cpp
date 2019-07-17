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

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("JacobianEvaluator::gradient")
{
    SECTION("constant + variable")
    {
        auto v = Tree::var();
        JacobianEvaluator e(v + 1.0, {{v.id(), 3.14}});

        auto g = e.gradient({1, 2, 3});
        REQUIRE(g.size() == 1);
        REQUIRE(g.count(v.id()) == 1);
        REQUIRE(g.at(v.id()) == Approx(1));
    }

    SECTION("x * variable")
    {
        auto v = Tree::var();
        JacobianEvaluator e(Tree::X() * v, {{v.id(), 1}});
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

        JacobianEvaluator e(a*1 + b*2 + c*3,
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        auto g = e.gradient({0, 0, 0});
        REQUIRE(g.at(a.id()) == Approx(1.0f));
        REQUIRE(g.at(b.id()) == Approx(2.0f));
        REQUIRE(g.at(c.id()) == Approx(3.0f));
    }

    SECTION("A truly hilarious number of variables")
    {
        std::list<Tree> ts;
        std::list<Tree> vars;
        std::map<Tree::Id, float> ids;

        // Silly work-around to avoid quadratic behavior in accumulated sums
        unsigned count = JacobianEvaluator::N * 4;
        for (unsigned i=0; i < count; ++i) {
            auto v = Tree::var();
            ts.push_back(v * i);
            vars.push_back(v);
            ids.insert({v.id(), static_cast<float>(i)});
        }
        while (ts.size() > 1) {
            for (auto itr = ts.begin(); itr != ts.end(); ++itr) {
                auto t = *itr;
                itr = ts.erase(itr);
                *itr = *itr + t;
            }
        }

        JacobianEvaluator e(*ts.begin(), ids);
        auto g = e.gradient({0, 0, 0});
        REQUIRE(g.size() == count - 1);
        for (auto& v : ids) {
            if (v.second > 0.0f) {
                REQUIRE(g.at(v.first) == v.second);
            }
        }
    }

    SECTION("Dependency on X/Y/Z")
    {
        auto a = Tree::var();
        auto b = Tree::var();
        auto c = Tree::var();

        JacobianEvaluator e(a*Tree::X() + b*Tree::Y() + c*Tree::Z(),
                {{a.id(), 3}, {b.id(), 7}, {c.id(), 5}});
        auto ds = e.deriv({0, 0, 0});
        REQUIRE(ds.x() == 3);
        REQUIRE(ds.y() == 7);
        REQUIRE(ds.z() == 5);

        auto g = e.gradient({1, 2, 4});
        REQUIRE(g.at(a.id()) == 1);
        REQUIRE(g.at(b.id()) == 2);
        REQUIRE(g.at(c.id()) == 4);
    }

    SECTION("CONST_VAR opcode")
    {
        auto v = Tree::var();
        JacobianEvaluator e((v + 1.0).makeVarsConstant(), {{v.id(), 3.14}});

        auto g = e.gradient({1, 2, 3});
        REQUIRE(g.size() == 1);
        REQUIRE(g.count(v.id()) == 1);
        REQUIRE(g.at(v.id()) == Approx(0));
    }
}

TEST_CASE("JacobianEvaluator::derivs")
{
        // Deliberately construct out of order
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        JacobianEvaluator e(
                a*1 + b*2 + c*3 + 4*Tree::X() + 5*Tree::Y() + 6*Tree::Z(),
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        auto ds = e.deriv({0, 0, 0});
        REQUIRE(ds.x() == 4);
        REQUIRE(ds.y() == 5);
        REQUIRE(ds.z() == 6);

        auto g = e.gradient({0, 0, 0});
        REQUIRE(g[a.id()] == 1);
        REQUIRE(g[b.id()] == 2);
        REQUIRE(g[c.id()] == 3);

        auto ds_ = e.deriv({0, 0, 0});
        REQUIRE(ds_.x() == 4);
        REQUIRE(ds_.y() == 5);
        REQUIRE(ds_.z() == 6);

}
