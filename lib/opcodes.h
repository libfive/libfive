#pragma once

enum Opcode
{
    INVALID,

    OP_NUM,
    OP_X,
    OP_Y,
    OP_Z,
    NO_ARGUMENTS,

    OP_SQRT,
    ONE_ARGUMENT,

    OP_ADD,
    OP_MUL,
    OP_MIN,
    OP_MAX,
    OP_SUB,
    OP_DIV,
    TWO_ARGUMENTS,
};
