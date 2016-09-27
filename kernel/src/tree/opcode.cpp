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
#include <boost/bimap.hpp>

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
        case POW:
        case NTH_ROOT:
        case MOD:
        case NANFILL:
        case AFFINE_VEC:
            return 2;

        case INVALID: // fallthrough
        case DUMMY_A:
        case DUMMY_B:
        case LAST_OP: return -1;
    }
}

std::string Opcode::to_str(Opcode op)
{
    switch (op)
    {
        case Opcode::CONST: return "const";
        case Opcode::AFFINE_VEC: return "affine-vec";
        case Opcode::DUMMY_A: return "dummy-a";
        case Opcode::DUMMY_B: return "dummy-b";
        case Opcode::LAST_OP: return "last-op";
        case Opcode::INVALID: return "invalid";
        case Opcode::VAR_X: return "X";
        case Opcode::VAR_Y: return "Y";
        case Opcode::VAR_Z: return "Z";
        case Opcode::ADD: return "add";
        case Opcode::MUL: return "mul";
        case Opcode::MIN: return "min";
        case Opcode::MAX: return "max";
        case Opcode::SUB: return "sub";
        case Opcode::DIV: return "div";
        case Opcode::ATAN2: return "atan2";
        case Opcode::POW: return "pow";
        case Opcode::NTH_ROOT: return "nth-root";
        case Opcode::MOD: return "mod";
        case Opcode::NANFILL: return "nan-fill";
        case Opcode::SQUARE: return "square";
        case Opcode::SQRT: return "sqrt";
        case Opcode::ABS: return "abs";
        case Opcode::NEG: return "neg";
        case Opcode::SIN: return "sin";
        case Opcode::COS: return "cos";
        case Opcode::TAN: return "tan";
        case Opcode::ASIN: return "asin";
        case Opcode::ACOS: return "acos";
        case Opcode::ATAN: return "atan";
        case Opcode::EXP: return "exp";
    }
}

Opcode::Opcode Opcode::from_str(std::string s)
{
    // Lazy initialization of string -> Opcode map
    static std::map<std::string, Opcode> inverse;
    if (inverse.size() == 0)
    {
        for (unsigned i=0; i <= LAST_OP; ++i)
        {
            inverse[to_str(Opcode(i))] = Opcode(i);
        }
    }

    auto itr = inverse.find(s);
    return itr != inverse.end() ? itr->second : INVALID;
}
