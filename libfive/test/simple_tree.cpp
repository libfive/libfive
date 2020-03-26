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
