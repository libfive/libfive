#include "ao/core/atom.hpp"
#include "ao/core/token.hpp"

/*  Compile-time macro to check array size  */
#define CHECK(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

Atom::Atom(Token* t)
    : op(t->op), value(t->value), mutable_value(nan("")), flags(0),
      a(t->a ? t->a->atom : nullptr),
      b(t->b ? t->b->atom : nullptr)
{
    // Compile-time detection of error conditions
    CHECK(ATOM_ARRAY_BYTES % sizeof(double));
    CHECK(ATOM_ARRAY_BYTES % sizeof(Interval));
    CHECK(ATOM_ARRAY_BYTES % sizeof(Gradient));
    CHECK(sizeof(Interval) == sizeof(double));
    CHECK(sizeof(Interval) == sizeof(Gradient));

    // Assert that children have atom pointers populated
    assert(t->a ? t->a->atom != nullptr : true);
    assert(t->b ? t->b->atom != nullptr : true);

    // Assert that this token hasn't already been added to the tree
    assert(t->atom == nullptr);

    t->atom = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), mutable_value(nan("")), flags(0), a(a), b(b)
{
    // Nothing to do here
}

Atom::Atom(double value)
    : op(OP_MUTABLE), value(nan("")), mutable_value(value), flags(0),
      a(nullptr), b(nullptr)
{
    // Nothing to do here
}
