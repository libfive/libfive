/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include <array>
#include <future>

#include "libfive/tree/simple_tree.hpp"
#include "util/oracles.hpp"

using namespace libfive;

TEST_CASE("SimpleTree: basic operations")
{
    auto t = SimpleTree::X() + 1;
    REQUIRE(t.op() == Opcode::OP_ADD);
    REQUIRE(t.lhs().op() == Opcode::VAR_X);
    REQUIRE(t.rhs().op() == Opcode::CONSTANT);
    REQUIRE(t.rhs().value() == 1);
}

TEST_CASE("SimpleTree: clone")
{
    auto t = SimpleTree::X();
    auto y = SimpleTree::Y();
    for (unsigned i=0; i < 32768; ++i) {
        t = t + y * i;
    }
    auto z = t.clone();
    REQUIRE(z.op() == Opcode::OP_ADD);
    REQUIRE(z.lhs().op() == Opcode::OP_ADD);
    REQUIRE(z.rhs().op() == Opcode::OP_MUL);
    REQUIRE(z.rhs().lhs().op() == Opcode::VAR_Y);
    REQUIRE(z.rhs().rhs().op() == Opcode::CONSTANT);
    REQUIRE(z.lhs().rhs().lhs().op() == Opcode::VAR_Y);
    REQUIRE(z.lhs().rhs().rhs().op() == Opcode::CONSTANT);
}

TEST_CASE("SimpleTree: remap")
{
    auto t = SimpleTree::X();
    auto y = SimpleTree::Y();
    for (unsigned i=0; i < 32768; ++i) {
        t = t + y * i;
    }
    auto z = t.remap(SimpleTree::Z(), SimpleTree::X(), SimpleTree::Y());

    // Make sure the original hasn't changed
    REQUIRE(t.op() == Opcode::OP_ADD);
    REQUIRE(t.lhs().op() == Opcode::OP_ADD);
    REQUIRE(t.rhs().op() == Opcode::OP_MUL);
    REQUIRE(t.rhs().lhs().op() == Opcode::VAR_Y);
    REQUIRE(t.rhs().rhs().op() == Opcode::CONSTANT);
    REQUIRE(t.lhs().rhs().lhs().op() == Opcode::VAR_Y);
    REQUIRE(t.lhs().rhs().rhs().op() == Opcode::CONSTANT);

    // Check that the remapping went through
    REQUIRE(z.op() == Opcode::OP_ADD);
    REQUIRE(z.lhs().op() == Opcode::OP_ADD);
    REQUIRE(z.rhs().op() == Opcode::OP_MUL);
    REQUIRE(z.rhs().lhs().op() == Opcode::VAR_X);
    REQUIRE(z.rhs().rhs().op() == Opcode::CONSTANT);
    REQUIRE(z.lhs().rhs().lhs().op() == Opcode::VAR_X);
    REQUIRE(z.lhs().rhs().rhs().op() == Opcode::CONSTANT);
}
