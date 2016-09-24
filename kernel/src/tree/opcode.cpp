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

static boost::bimap<std::string, Opcode::Opcode> opcode_strs;
static bool opcode_strs_initialized = false;

static void initialize_opcode_strs()
{
    if (opcode_strs_initialized == false)
    {
        std::vector<std::pair<std::string, Opcode::Opcode>> keys = {
            {"const", Opcode::CONST},
            {"affine-vec", Opcode::AFFINE_VEC},
            {"dummy-a", Opcode::DUMMY_A},
            {"dummy-b", Opcode::DUMMY_B},
            {"last-op", Opcode::LAST_OP},
            {"invalid", Opcode::INVALID},
            {"X", Opcode::VAR_X},
            {"Y", Opcode::VAR_Y},
            {"Z", Opcode::VAR_Z},
            {"add", Opcode::ADD},
            {"mul", Opcode::MUL},
            {"min", Opcode::MIN},
            {"max", Opcode::MAX},
            {"sub", Opcode::SUB},
            {"div", Opcode::DIV},
            {"atan2", Opcode::ATAN2},
            {"pow", Opcode::POW},
            {"mod", Opcode::MOD},
            {"nan-fill", Opcode::NANFILL},
            {"square", Opcode::SQUARE},
            {"sqrt", Opcode::SQRT},
            {"abs", Opcode::ABS},
            {"neg", Opcode::NEG},
            {"sin", Opcode::SIN},
            {"cos", Opcode::COS},
            {"tan", Opcode::TAN},
            {"asin", Opcode::ASIN},
            {"acos", Opcode::ACOS},
            {"atan", Opcode::ATAN},
            {"exp", Opcode::EXP}};

        for (auto k : keys)
        {
            opcode_strs.insert({k.first, k.second});
        }

        opcode_strs_initialized = true;
    }
};

std::string Opcode::to_str(Opcode op)
{
    initialize_opcode_strs();
    return opcode_strs.right.at(op);
}

Opcode::Opcode Opcode::from_str(std::string s)
{
    initialize_opcode_strs();
    auto itr = opcode_strs.left.find(s);
    if (itr != opcode_strs.left.end())
    {
        return itr->second;
    }
    return INVALID;
}
