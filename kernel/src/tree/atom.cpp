#include <cassert>

#include "ao/kernel/tree/atom.hpp"
#include "ao/kernel/tree/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Atom::Atom(const Token* t, std::unordered_map<const Token*, Atom*>& atoms)
    : op(t->op), value(t->value),
      a(t->a ? atoms[t->a] : nullptr),
      b(t->b ? atoms[t->b] : nullptr)
{
    // Assert that children have atom pointers populated
    assert(t->a ? atoms.count(t->a) : true);
    assert(t->b ? atoms.count(t->b) : true);

    // Assert that this token hasn't already been added to the tree
    assert(atoms[t] == nullptr);

    atoms[t] = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), a(a), b(b)
{
    // Nothing to do here
}

Atom::Atom(float d)
    : op(OP_MUTABLE), value(d), a(nullptr), b(nullptr)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

std::ostream& operator<<(std::ostream& os, const Atom& atom)
{
    switch (atom.op)
    {
        case OP_ADD:    os << "(" << *atom.a << " + " << *atom.b << ")"; break;
        case OP_MUL:    os << "(" << *atom.a << " * " << *atom.b << ")"; break;
        case OP_MIN:    os << "min(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_MAX:    os << "max(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_SUB:    os << "(" << *atom.a << " - " << *atom.b << ")"; break;
        case OP_DIV:    os << "(" << *atom.a << " / " << *atom.b << ")"; break;
        case OP_ATAN2:  os << "atan2(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_MOD:    os << "mod(" << *atom.a << ", " << *atom.b << ")"; break;

        case OP_SQUARE: os << "square(" << *atom.a << ")"; break;
        case OP_SQRT:   os << "sqrt(" << *atom.a << ")"; break;
        case OP_NEG:    os << "(-" << *atom.a << ")"; break;
        case OP_ABS:    os << "abs(" << *atom.a << ")"; break;
        case OP_SIN:    os << "sin(" << *atom.a << ")"; break;
        case OP_COS:    os << "cos(" << *atom.a << ")"; break;
        case OP_TAN:    os << "tan(" << *atom.a << ")"; break;
        case OP_ASIN:    os << "asin(" << *atom.a << ")"; break;
        case OP_ACOS:    os << "acos(" << *atom.a << ")"; break;
        case OP_ATAN:    os << "atan(" << *atom.a << ")"; break;
        case OP_EXP:    os << "exp(" << *atom.a << ")"; break;

        case OP_CONST:  os << atom.value; break;
        case OP_MUTABLE:  os << atom.value; break;
        case OP_X:      os << "X"; break;
        case OP_Y:      os << "Y"; break;
        case OP_Z:      os << "Z"; break;

        case LAST_OP:   // Fallthrough!
        case OP_A:
        case OP_B:
        case INVALID:   assert(false);
    }
    return os;
}
