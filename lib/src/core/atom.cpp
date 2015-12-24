#include "ao/core/atom.hpp"
#include "ao/core/token.hpp"

////////////////////////////////////////////////////////////////////////////////
/*  Compile-time checking of object sizes  */
#define CHECK(condition) ((void)sizeof(char[1 - 2*!!(condition)]))
void CHECK_SIZES()
{
    CHECK(RESULT_ARRAY_BYTES % sizeof(double));
    CHECK(RESULT_ARRAY_BYTES % sizeof(Interval));
    CHECK(RESULT_ARRAY_BYTES % sizeof(Gradient));
    CHECK(sizeof(Interval) == sizeof(double));
    CHECK(sizeof(Interval) == sizeof(Gradient));
}

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
