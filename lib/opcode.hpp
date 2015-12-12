#pragma once

enum Opcode
{
    INVALID,

    OP_CONST,
    OP_X,
    OP_Y,
    OP_Z,

    OP_SQRT,
    OP_NEG,

    OP_ADD,
    OP_MUL,
    OP_MIN,
    OP_MAX,
    OP_SUB,
    OP_DIV,

    LAST_OP,
};
