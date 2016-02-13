#pragma once

enum Opcode
{
    INVALID,

    OP_CONST,
    OP_MUTABLE,
    OP_X,
    OP_Y,
    OP_Z,

    OP_SQUARE,
    OP_SQRT,
    OP_NEG,
    OP_ABS,
    OP_SIN,
    OP_COS,
    OP_TAN,
    OP_ASIN,
    OP_ACOS,
    OP_ATAN,

    OP_ADD,
    OP_MUL,
    OP_MIN,
    OP_MAX,
    OP_SUB,
    OP_DIV,
    OP_ATAN2,

    // Dummy opcodes used to select the left or right-hand side of a function
    // (used when one of the children is disabled)
    OP_A,
    OP_B,
    LAST_OP,
};
