#include <cmath>
#include <algorithm>

#include "atom.hpp"
#include "token.hpp"

/*  Compile-time macro to check array size  */
#define CHECK_ARRAY_SIZE(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

Atom::Atom(Token* t)
    : op(t->op), value(t->value), flags(0),
      a(t->a ? t->a->atom : nullptr),
      b(t->b ? t->b->atom : nullptr)
{
    CHECK_ARRAY_SIZE(ATOM_ARRAY_BYTES % sizeof(double));
    CHECK_ARRAY_SIZE(ATOM_ARRAY_BYTES % sizeof(Interval));
    CHECK_ARRAY_SIZE(ATOM_ARRAY_BYTES % sizeof(Gradient));

    // Assert that children have atom pointers populated
    assert(t->a ? t->a->atom != nullptr : true);
    assert(t->b ? t->b->atom != nullptr : true);

    // Assert that this token hasn't already been added to the tree
    assert(t->atom == nullptr);

    t->atom = this;
}
