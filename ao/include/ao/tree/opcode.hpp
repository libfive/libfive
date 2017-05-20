#pragma once

#include <cstdlib>
#include <string>

namespace Kernel {

namespace Opcode
{
enum Opcode
{
    INVALID,

    CONST,
    VAR_X,
    VAR_Y,
    VAR_Z,
    VAR,
    CONST_VAR,

    SQUARE,
    SQRT,
    NEG,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    EXP,

    ADD,
    MUL,
    MIN,
    MAX,
    SUB,
    DIV,
    ATAN2,
    POW,
    NTH_ROOT,
    MOD,
    NANFILL,

    LAST_OP,
};

size_t args(Opcode op);
std::string to_str(Opcode op);
Opcode from_str(std::string s);
bool isCommutative(Opcode op);

}


}   // namespace Kernel
