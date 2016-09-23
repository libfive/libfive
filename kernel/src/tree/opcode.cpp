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

#include "ao/kernel/tree/opcode.hpp"

size_t Opcode::args(Opcode op)
{
    switch (op)
    {
        case CONST: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
            return 0;

        case SQUARE: // fallthrough
        case SQRT:
        case NEG:
        case ABS:
        case SIN:
        case COS:
        case TAN:
        case ASIN:
        case ACOS:
        case ATAN:
        case EXP:
            return 1;

        case ADD: // fallthrough
        case MUL:
        case MIN:
        case MAX:
        case SUB:
        case DIV:
        case ATAN2:
        case MOD:
        case NANFILL:
        case AFFINE_VEC:
            return 2;

        default:
        case INVALID: // fallthrough
        case DUMMY_A:
        case DUMMY_B:
        case LAST_OP: return -1;
    }
}

