/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <algorithm>
#include <cmath>
#include <cassert>

#include "ao/kernel/tree/token.hpp"

Token::Token(Opcode op, Token* a, Token* b)
    : op(op), weight(std::max(a ? a->weight + 1 : 0,
                              b ? b->weight + 1 : 0)),
      value(std::nan("")), a(a), b(b)
{
    // Nothing to do here
}

Token::Token(float v)
    : op(CONST), weight(0), value(v), a(nullptr), b(nullptr)
{
    // Nothing to do here
}

size_t Token::args(Opcode op)
{
    switch (op)
    {
        case CONST: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
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
        case OP_NANFILL:
        case AFFINE_VEC:
            return 2;

        case INVALID: // fallthrough
        case DUMMY_A:
        case DUMMY_B:
        case LAST_OP: return -1;
    }
}

glm::vec4 Token::getAffine(bool* success)
{
    if (op != AFFINE_VEC)
    {
        if (success != nullptr)
        {
            *success = false;
        }
        return {};
    }

    if (success != nullptr)
    {
        *success = true;
    }

    return {a->a->b->value, a->b->b->value,
            b->a->b->value, b->b->value};
}
