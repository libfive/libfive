/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <iostream>

// Forward declarations
namespace libfive {
class Tree;

#define LIBFIVE_TREE_OPERATORS \
OP_UNARY(square, OP_SQUARE) \
OP_UNARY(sqrt, OP_SQRT) \
OP_UNARY(abs, OP_ABS) \
OP_UNARY(operator-, OP_NEG) \
OP_UNARY(sin, OP_SIN) \
OP_UNARY(cos, OP_COS) \
OP_UNARY(tan, OP_TAN) \
OP_UNARY(asin, OP_ASIN) \
OP_UNARY(acos, OP_ACOS) \
OP_UNARY(atan, OP_ATAN) \
OP_UNARY(log, OP_LOG) \
OP_UNARY(exp, OP_EXP) \
OP_BINARY(operator+, OP_ADD) \
OP_BINARY(operator*, OP_MUL) \
OP_BINARY(min, OP_MIN) \
OP_BINARY(max, OP_MAX) \
OP_BINARY(operator-, OP_SUB) \
OP_BINARY(operator/, OP_DIV) \
OP_BINARY(atan2, OP_ATAN2) \
OP_BINARY(pow, OP_POW) \
OP_BINARY(nth_root, OP_NTH_ROOT) \
OP_BINARY(mod, OP_MOD) \
OP_BINARY(nanfill, OP_NANFILL) \
OP_BINARY(compare, OP_COMPARE)

// Mass-produce declarations for overloaded operations
#define OP_UNARY(OP, C)    libfive::Tree OP(const libfive::Tree& a);
#define OP_BINARY(OP, C)   libfive::Tree OP(const libfive::Tree& a,\
                                                  const libfive::Tree& b);
LIBFIVE_TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY

/*  Prints the tree to the given ostream. */
std::ostream& operator<<(std::ostream& stream, const libfive::Tree& tree);

}   // namespace libfive
