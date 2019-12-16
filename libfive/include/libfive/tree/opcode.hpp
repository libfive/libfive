/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <cstdlib>
#include <string>

namespace libfive {

namespace Opcode
{
// To create a new opcode, add it to the relevant section with the
// next-highest value and increment LAST_OP by one.  This is necessary
// to avoid changing the meanings of opcodes in previously saved files.
#ifndef LIBFIVE_PACKED_OPCODES
#define OPCODES \
    OPCODE(INVALID, 0)      \
                            \
    OPCODE(CONSTANT, 1)     \
    OPCODE(VAR_X, 2)        \
    OPCODE(VAR_Y, 3)        \
    OPCODE(VAR_Z, 4)        \
    OPCODE(VAR_FREE, 5)     \
    OPCODE(CONST_VAR, 6)    \
                            \
    OPCODE(OP_SQUARE, 7)    \
    OPCODE(OP_SQRT, 8)      \
    OPCODE(OP_NEG, 9)       \
    OPCODE(OP_SIN, 10)      \
    OPCODE(OP_COS, 11)      \
    OPCODE(OP_TAN, 12)      \
    OPCODE(OP_ASIN, 13)     \
    OPCODE(OP_ACOS, 14)     \
    OPCODE(OP_ATAN, 15)     \
    OPCODE(OP_EXP, 16)      \
    OPCODE(OP_ABS, 28)      \
    OPCODE(OP_LOG, 30)      \
    OPCODE(OP_RECIP, 29)    \
                            \
    OPCODE(OP_ADD, 17)      \
    OPCODE(OP_MUL, 18)      \
    OPCODE(OP_MIN, 19)      \
    OPCODE(OP_MAX, 20)      \
    OPCODE(OP_SUB, 21)      \
    OPCODE(OP_DIV, 22)      \
    OPCODE(OP_ATAN2, 23)    \
    OPCODE(OP_POW, 24)      \
    OPCODE(OP_NTH_ROOT, 25) \
    OPCODE(OP_MOD, 26)      \
    OPCODE(OP_NANFILL, 27)  \
    OPCODE(OP_COMPARE, 31)  \
                            \
    OPCODE(ORACLE, 32)      \
    /* end of opcodes */
#else
#define OPCODES \
    OPCODE(INVALID, 0)      \
                            \
    OPCODE(CONSTANT, 1)     \
    OPCODE(VAR_X, 2)        \
    OPCODE(VAR_Y, 3)        \
    OPCODE(VAR_Z, 4)        \
    OPCODE(VAR_FREE, 5)     \
    OPCODE(CONST_VAR, 6)    \
                            \
    OPCODE(OP_SQUARE, 7)    \
    OPCODE(OP_SQRT, 8)      \
    OPCODE(OP_NEG, 9)       \
    OPCODE(OP_SIN, 10)      \
    OPCODE(OP_COS, 11)      \
    OPCODE(OP_TAN, 12)      \
    OPCODE(OP_ASIN, 13)     \
    OPCODE(OP_ACOS, 14)     \
    OPCODE(OP_ATAN, 15)     \
    OPCODE(OP_EXP, 16)      \
    OPCODE(OP_ABS, 17)      \
    OPCODE(OP_LOG, 18)      \
    OPCODE(OP_RECIP, 19)    \
                            \
    OPCODE(OP_ADD, 20)      \
    OPCODE(OP_MUL, 21)      \
    OPCODE(OP_MIN, 22)      \
    OPCODE(OP_MAX, 23)      \
    OPCODE(OP_SUB, 24)      \
    OPCODE(OP_DIV, 25)      \
    OPCODE(OP_ATAN2, 26)    \
    OPCODE(OP_POW, 27)      \
    OPCODE(OP_NTH_ROOT, 28) \
    OPCODE(OP_MOD, 29)      \
    OPCODE(OP_NANFILL, 30)  \
    OPCODE(OP_COMPARE, 31)  \
                            \
    OPCODE(ORACLE, 32)      \
    /* end of opcodes */
#endif

enum Opcode {
#define OPCODE(s, i) s=i,
    OPCODES
#undef OPCODE
    LAST_OP=33,
};

size_t args(Opcode op);

/*
 *  Converts to the bare enum string (e.g. ATAN2)
 */
std::string toString(Opcode op);

/*
 *  Converts to a operator string (+, -, etc) or the function name
 *  (atan, cos, etc)
 */
std::string toOpString(Opcode op);

/*
 *  Returns a Scheme symbol-style string, e.g. lower-case
 *  with underscores replaced by dashes.
 */
std::string toScmString(Opcode op);

/*
 *  Converts from a Scheme symbol-style string to an enum value,
 *  return INVALID if there's no match.
 */
Opcode fromScmString(std::string s);

/*
 *  Returns true if the opcode is commutative (+, *, etc)
 */
bool isCommutative(Opcode op);

}


}   // namespace libfive
