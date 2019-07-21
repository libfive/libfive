/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <iostream>
#include <map>

#include <boost/algorithm/string.hpp>

#include "libfive/tree/opcode.hpp"

namespace libfive {

size_t Opcode::args(Opcode op)
{
    switch (op)
    {
        case CONSTANT: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
        case VAR_FREE:
        case ORACLE:
            return 0;

        case OP_SQUARE: // fallthrough
        case OP_SQRT:
        case OP_NEG:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_EXP:
        case OP_ABS:
        case OP_LOG:
        case OP_RECIP:
        case CONST_VAR:
            return 1;

        case OP_ADD: // fallthrough
        case OP_MUL:
        case OP_MIN:
        case OP_MAX:
        case OP_SUB:
        case OP_DIV:
        case OP_ATAN2:
        case OP_POW:
        case OP_NTH_ROOT:
        case OP_MOD:
        case OP_NANFILL:
        case OP_COMPARE:
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
            opcode_names[o.first] =
                boost::algorithm::starts_with(o.second, "OP_")
                    ? o.second.substr(3)
                    : o.second;
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

        case VAR_FREE: // fallthrough
        case OP_SQUARE:
        case OP_SQRT:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_ATAN2:
        case OP_EXP:
        case CONST_VAR:
        case OP_MIN:
        case OP_MAX:
        case OP_POW:
        case OP_NTH_ROOT:
        case OP_MOD:
        case OP_NANFILL:
        case OP_COMPARE:
        case OP_LOG:
        case OP_ABS:
            return toScmString(op);


        case OP_ADD:   return "+";
        case OP_MUL:   return "*";
        case OP_NEG:   // FALLTHROUGH
        case OP_SUB:   return "-";
        case OP_RECIP: // FALLTHROUGH
        case OP_DIV:   return "/";

        case INVALID: // fallthrough
        case CONSTANT:
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
        case CONSTANT: // fallthrough
        case VAR_X:
        case VAR_Y:
        case VAR_Z:
        case VAR_FREE:
        case ORACLE:
        case OP_SQUARE:
        case OP_SQRT:
        case OP_NEG:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_EXP:
        case OP_SUB:
        case OP_DIV:
        case OP_ATAN2:
        case OP_POW:
        case OP_NTH_ROOT:
        case OP_MOD:
        case OP_NANFILL:
        case OP_COMPARE:
        case INVALID:
        case OP_LOG:
        case OP_ABS:
        case OP_RECIP:
        case CONST_VAR:
        case LAST_OP:
            return false;

        case OP_ADD: // fallthrough
        case OP_MUL:
        case OP_MIN:
        case OP_MAX:
            return true;
    }
    assert(false);
    return false;
}

}   // namespace libfive
