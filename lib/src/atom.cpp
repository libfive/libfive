#include "atom.hpp"
#include "token.hpp"

/*  Compile-time macro to check array size  */
#define CHECK(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

Atom::Atom(Token* t)
    : op(t->op), value(t->value), flags(0),
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
