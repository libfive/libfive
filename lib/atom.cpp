#include <cmath>
#include <algorithm>

#include "atom.h"
#include "token.h"

Atom::Atom(Token* t)
    : op(t->op), value(t->value), flags(0),
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

void Atom::Result::set(const std::vector<double>& ds)
{
    assert(ds.size() <= ATOM_ARRAY_SIZE);
    std::copy(ds.begin(), ds.end(), d);
}

void Atom::Result::set(const std::vector<Interval>& is)
{
    assert(is.size() <= ATOM_ARRAY_SIZE / 2);
    std::copy(is.begin(), is.end(), i);
}
