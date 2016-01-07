#pragma once

enum Opcode
{
    INVALID,

    OP_CONST,
    OP_MUTABLE,
    OP_X,
    OP_Y,
    OP_Z,

    OP_SQRT,
    OP_NEG,
    OP_ABS,

    OP_ADD,
    OP_MUL,
    OP_MIN,
    OP_MAX,
    OP_SUB,
    OP_DIV,

    COND_LZ,

    LAST_OP,
};
