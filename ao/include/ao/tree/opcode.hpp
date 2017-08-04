#pragma once

#include <cstdlib>
#include <string>

namespace Kernel {

namespace Opcode
{

#define OPCODES \
    OPCODE(INVALID, 0)      \
                            \
    OPCODE(CONST, 1)        \
    OPCODE(VAR_X, 2)        \
    OPCODE(VAR_Y, 3)        \
    OPCODE(VAR_Z, 4)        \
    OPCODE(VAR, 5)          \
    OPCODE(CONST_VAR, 6)    \
                            \
    OPCODE(SQUARE, 7)       \
    OPCODE(SQRT, 8)         \
    OPCODE(NEG, 9)          \
    OPCODE(SIN, 10)         \
    OPCODE(COS, 11)         \
    OPCODE(TAN, 12)         \
    OPCODE(ASIN, 13)        \
    OPCODE(ACOS, 14)        \
    OPCODE(ATAN, 15)        \
    OPCODE(EXP, 16)         \
                            \
    OPCODE(ADD, 17)         \
    OPCODE(MUL, 18)         \
    OPCODE(MIN, 19)         \
    OPCODE(MAX, 20)         \
    OPCODE(SUB, 21)         \
    OPCODE(DIV, 22)         \
    OPCODE(ATAN2, 23)       \
    OPCODE(POW, 24)         \
    OPCODE(NTH_ROOT, 25)    \
    OPCODE(MOD, 26)         \
    OPCODE(NANFILL, 27)

enum Opcode {
#define OPCODE(s, i) s=i,
    OPCODES
#undef OPCODE
    LAST_OP,
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


}   // namespace Kernel
