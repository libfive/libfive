#include "ao/core/atom.hpp"
#include "ao/core/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Atom::Atom(Token* t)
    : op(t->op), value(t->value), mutable_value(nan("")),
      a(t->a ? t->a->atom : nullptr),
      b(t->b ? t->b->atom : nullptr)
{

    // Assert that children have atom pointers populated
    assert(t->a ? t->a->atom != nullptr : true);
    assert(t->b ? t->b->atom != nullptr : true);

    // Assert that this token hasn't already been added to the tree
    assert(t->atom == nullptr);

    t->atom = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), mutable_value(nan("")), a(a), b(b)
{
    // Nothing to do here
}

Atom::Atom(double value)
    : op(OP_MUTABLE), value(nan("")), mutable_value(value),
      a(nullptr), b(nullptr)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

bool Atom::checkDisabled()
{
    if (flags & ATOM_FLAG_IGNORED)
    {
        clearFlags();
        mutable_value = result.get<Interval>(0).lower();
        return true;
    }

    // For min and max operations, we may only need to keep one branch
    // active if it is decisively above or below the other branch.
    if (op == OP_MAX)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    else if (op == OP_MIN)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    // For other operations, we keep both branches active
    else
    {
        if (a)
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        if (b)
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    return true;
}
