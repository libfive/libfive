/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <iostream>
#include <map>

#include <boost/algorithm/string.hpp>

#include "libfive/tree/opcode.hpp"

namespace Kernel {

size_t Opcode::args(Opcode op)
{
    switch (op)
    {
        case CONST: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
        case VAR:
        case ORACLE:
            return 0;

        case SQUARE: // fallthrough
        case SQRT:
        case NEG:
        case SIN:
        case COS:
        case TAN:
        case ASIN:
        case ACOS:
        case ATAN:
        case EXP:
        case CONST_VAR:
        case ABS:
        case LOG:
        case RECIP:
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
        case COMPARE:
            return 2;

        case INVALID: // fallthrough
        case LAST_OP: return -1;
    }
    assert(false); /* All enumeration values must be handled */
    return -1;
}

const static std::map<Opcode::Opcode, std::string> _opcode_names = {
#define OPCODE(s, i) {Opcode::s, #s},
    OPCODES
#undef OPCODE
};
static std::string opcode_names[Opcode::LAST_OP];

static void buildNames()
{
    if (opcode_names[0].empty())
    {
        for (auto& o : _opcode_names)
        {
            opcode_names[o.first] = o.second;
        }
    }
}

std::string Opcode::toString(Opcode op)
{
    buildNames();

    if (op >= LAST_OP || op < 0)
    {
        std::cerr << "Opcode::toString: Invalid opcode " << op << std::endl;
        return "";
    }
    return opcode_names[op];
}

std::string Opcode::toScmString(Opcode op)
{
    buildNames();

    if (op >= LAST_OP || op < 0)
    {
        std::cerr << "Opcode::toString: Invalid opcode " << op << std::endl;
        return "";
    }

    auto s = opcode_names[op];
    std::replace(s.begin(), s.end(), '_', '-');
    boost::algorithm::to_lower(s);
    return s;
}

Opcode::Opcode Opcode::fromScmString(std::string s)
{
    // Lazy initialization of string -> Opcode map
    static std::map<std::string, Opcode> inverse;
    if (inverse.size() == 0)
    {
        for (unsigned i=0; i < LAST_OP; ++i)
        {
            inverse[toScmString(Opcode(i))] = Opcode(i);
        }
    }

    // Be liberal in what you accept
    boost::algorithm::to_lower(s);

    auto itr = inverse.find(s);
    return itr != inverse.end() ? itr->second : INVALID;
}

std::string Opcode::toOpString(Opcode op)
{
    switch (op)
    {
        case VAR_X: return "x";
        case VAR_Y: return "y";
        case VAR_Z: return "z";

        case VAR:  // fallthrough
        case SQUARE: 
        case SQRT:
        case SIN:
        case COS:
        case TAN:
        case ASIN:
        case ACOS:
        case ATAN:
        case ATAN2:
        case EXP:
        case CONST_VAR:
        case MIN:
        case MAX:
        case POW:
        case NTH_ROOT:
        case MOD:
        case NANFILL:
        case COMPARE:
        case LOG:
        case ABS:
            return toScmString(op);


        case ADD:   return "+";
        case MUL:   return "*";
        case NEG:   // FALLTHROUGH
        case SUB:   return "-";
        case RECIP: // FALLTHROUGH
        case DIV:   return "/";

        case INVALID: // fallthrough
        case CONST:
        case ORACLE:
        case LAST_OP: return "";
    }
    assert(false);
    return "";
}

bool Opcode::isCommutative(Opcode op)
{
    switch (op)
    {
        case CONST: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
        case VAR:
        case ORACLE:
        case SQUARE:
        case SQRT:
        case NEG:
        case SIN:
        case COS:
        case TAN:
        case ASIN:
        case ACOS:
        case ATAN:
        case EXP:
        case SUB:
        case DIV:
        case ATAN2:
        case POW:
        case NTH_ROOT:
        case MOD:
        case NANFILL:
        case COMPARE:
        case INVALID:
        case LOG:
        case ABS:
        case RECIP:
        case CONST_VAR:
        case LAST_OP:
            return false;

        case ADD: // fallthrough
        case MUL:
        case MIN:
        case MAX:
            return true;
    }
    assert(false);
    return false;
}

}   // namespace Kernel
