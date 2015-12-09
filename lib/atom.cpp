#include <cmath>

#include "atom.h"

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), a(a), b(b)
{
    // Nothing to do here
}

Atom::Atom(double v)
    : op(OP_CONST), value(v), a(nullptr), b(nullptr)
{
    // Nothing to do here
}
