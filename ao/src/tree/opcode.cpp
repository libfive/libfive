#include <map>
#include <boost/algorithm/string.hpp>

#include "ao/tree/opcode.hpp"

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
            return 2;

        case INVALID: // fallthrough
        case LAST_OP: return -1;
    }
    assert(false); /* All enumeration values must be handled */
    return -1;
}

std::string Opcode::to_str(Opcode op)
{
    switch (op)
    {
        case Opcode::CONST: return "const";
        case Opcode::CONST_VAR: return "const";
        case Opcode::LAST_OP: return "last-op";
        case Opcode::INVALID: return "invalid";
        case Opcode::VAR_X: return "x";
        case Opcode::VAR_Y: return "y";
        case Opcode::VAR_Z: return "z";
        case Opcode::VAR: return "var";
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
        case Opcode::NEG: return "neg";
        case Opcode::SIN: return "sin";
        case Opcode::COS: return "cos";
        case Opcode::TAN: return "tan";
        case Opcode::ASIN: return "asin";
        case Opcode::ACOS: return "acos";
        case Opcode::ATAN: return "atan";
        case Opcode::EXP: return "exp";
    }
    assert(false); /* All enumeration values must be handled */
    return "";
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

    boost::algorithm::to_lower(s);
    auto itr = inverse.find(s);
    return itr != inverse.end() ? itr->second : INVALID;
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
        case INVALID:
        case CONST_VAR: // fallthrough
        case LAST_OP:
            return false;

        case ADD: // fallthrough
        case MUL:
        case MIN:
        case MAX:
            return true;
    }

}

}   // namespace Kernel
