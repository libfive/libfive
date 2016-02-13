#include <algorithm>
#include <cmath>

#include "ao/kernel/tree/token.hpp"

Token::Token(Opcode op, Token* a, Token* b)
    : op(op), weight(std::max(a ? a->weight + 1 : 0,
                              b ? b->weight + 1 : 0)),
      value(nan("")), a(a), b(b)
{
    // Nothing to do here
}

Token::Token(float v)
    : op(OP_CONST), weight(0), value(v), a(nullptr), b(nullptr)
{
    // Nothing to do here
}

size_t Token::args(Opcode op)
{
    switch (op)
    {
        case OP_CONST: // fallthrough
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
            return 0;

        case OP_SQUARE: // fallthrough
        case OP_SQRT:
        case OP_NEG:
        case OP_ABS:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_EXP:
            return 1;

        case OP_ADD: // fallthrough
        case OP_MUL:
        case OP_MIN:
        case OP_MAX:
        case OP_SUB:
        case OP_DIV:
        case OP_ATAN2:
        case OP_MOD:
            return 2;

        case INVALID: // fallthrough
        case OP_A:
        case OP_B:
        case LAST_OP: return -1;
    }
}
