#include <cassert>

#include "ao/tree/atom.hpp"
#include "ao/tree/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Atom::Atom(const Token* t, std::unordered_map<const Token*, Atom*>& atoms)
    : op(t->op), value(t->value),
      a(t->a ? atoms[t->a] : nullptr),
      b(t->b ? atoms[t->b] : nullptr),
      cond(t->cond ? atoms[t->cond] : nullptr)
{
    // Assert that children have atom pointers populated
    assert(t->a ? atoms.count(t->a) : true);
    assert(t->b ? atoms.count(t->b) : true);
    assert(t->cond ? atoms.count(t->cond) : true);

    // Assert that this token hasn't already been added to the tree
    assert(atoms[t] == nullptr);

    atoms[t] = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), a(a), b(b), cond(nullptr)
{
    // Nothing to do here
}

Atom::Atom(double d)
    : op(OP_MUTABLE), value(d), a(nullptr), b(nullptr), cond(nullptr)
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
        case OP_SQRT:   os << "sqrt(" << *atom.a << ")"; break;
        case OP_NEG:    os << "(-" << *atom.a << ")"; break;
        case OP_CONST:  os << atom.value; break;
        case OP_MUTABLE:  os << atom.value; break;
        case OP_X:      os << "X"; break;
        case OP_Y:      os << "Y"; break;
        case OP_Z:      os << "Z"; break;

        case COND_LZ:   os << "(" << *atom.cond << " < 0 ? "
                                  << *atom.a << " : " << *atom.b << ")"; break;

        case LAST_OP:   // Fallthrough!
        case INVALID:   assert(false);
    }
    return os;
}
