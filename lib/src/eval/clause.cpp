#include "ao/eval/clause.hpp"
#include "ao/tree/atom.hpp"

////////////////////////////////////////////////////////////////////////////////

Clause::Clause(const Atom* m,
               std::unordered_map<const Atom*, Clause*>& clauses)
    : op(m->op), value(m->value), mutable_value(m->value),
      a(m->a ? clauses[m->a] : nullptr), b(m->b ? clauses[m->b] : nullptr)
{
    // Assert that children have been added to the clause map
    assert(m->a ? clauses.count(m->a) : true);
    assert(m->b ? clauses.count(m->b) : true);

    // Assert that this atom hasn't already been added to the tree
    assert(clauses[m] == nullptr);

    clauses[m] = this;

    if (op == OP_CONST || op == OP_MUTABLE)
    {
        result.fill(value);
    }
}

bool Clause::checkDisabled()
{
    if (flags & CLAUSE_FLAG_IGNORED)
    {
        clearFlags();
        return true;
    }

    // For min and max operations, we may only need to keep one branch
    // active if it is decisively above or below the other branch.
    if (op == OP_MAX)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
    }
    else if (op == OP_MIN)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
    }
    // For other operations, we keep both branches active
    else
    {
        if (a)
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        if (b)
        {
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
    }
    return false;
}

////////////////////////////////////////////////////////////////////////////////

void Clause::disable()
{
    mutable_value = result.get<Interval>(0).lower();

    for (size_t i=0; i < result.count<__m256>(); ++i)
    {
        result.set(_mm256_set1_ps(mutable_value), i);
    }

    setFlag(CLAUSE_FLAG_DISABLED);
}

void Clause::enable()
{
    clearFlag(CLAUSE_FLAG_DISABLED);
}
