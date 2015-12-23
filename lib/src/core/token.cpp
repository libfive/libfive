#include <algorithm>
#include <cmath>

#include "ao/core/token.hpp"

Token::Token(Opcode op, Token* a, Token* b)
    : op(op), weight(std::max(a ? a->weight + 1 : 0,
                              b ? b->weight + 1 : 0)),
      value(nan("")), a(a), b(b), found(false), atom(nullptr)
{
    // Nothing to do here
}

Token::Token(double v)
    : op(OP_CONST), weight(0), value(v), a(nullptr), b(nullptr),
      found(false), atom(nullptr)
{
    // Nothing to do here
}

size_t Token::args(Opcode op)
{
    switch (op)
    {
        case OP_CONST: return 0;
        case OP_MUTABLE: return 0;
        case OP_X: return 0;
        case OP_Y: return 0;
        case OP_Z: return 0;

        case OP_SQRT: return 1;
        case OP_NEG: return 1;

        case OP_ADD: return 2;
        case OP_MUL: return 2;
        case OP_MIN: return 2;
        case OP_MAX: return 2;
        case OP_SUB: return 2;
        case OP_DIV: return 2;

        case INVALID: // fallthrough
        case LAST_OP: return -1;
    }
}
