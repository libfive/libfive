/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "ao/kernel/eval/clause.hpp"
#include "ao/kernel/tree/atom.hpp"

////////////////////////////////////////////////////////////////////////////////

Clause::Clause(const Atom* m,
               std::unordered_map<const Atom*, Clause*>& clauses)
    : op(m->op), value(m->value), mutable_value(m->value),
      a(m->a ? clauses[m->a] : nullptr), b(m->b ? clauses[m->b] : nullptr)
{
    // Assert that children have been added to the clause map
    assert(m->a ? clauses.count(m->a) : true);
    assert(m->b ? clauses.count(m->b) : true);

    // Helper function to copy a set of result pointers if a clause is present
    auto load_ptrs = [](ResultPtrs* p, Clause* c)
    {
        if (c)
        {
            p->f = c->result.f;
            p->dx = c->result.dx;
            p->dy = c->result.dy;
            p->dz = c->result.dz;
        }
        else
        {
            p->f = nullptr;
            p->dx = nullptr;
            p->dy = nullptr;
            p->dz = nullptr;
        }
#ifdef __AVX__
        if (c)
        {
            p->mf = c->result.mf;
            p->mdx = c->result.mdx;
            p->mdy = c->result.mdy;
            p->mdz = c->result.mdz;
        }
        else
        {
            p->mf = nullptr;
            p->mdx = nullptr;
            p->mdy = nullptr;
            p->mdz = nullptr;
        }
#endif
    };
    load_ptrs(&ptrs.a, a);
    load_ptrs(&ptrs.b, b);

    // Assert that this atom hasn't already been added to the tree
    assert(clauses[m] == nullptr);

    clauses[m] = this;

    if (op == OP_CONST || op == AFFINE_VALUE)
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
        if (a->result.i.lower() >= b->result.i.upper())
        {
            a->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else if (b->result.i.lower() >= a->result.i.upper())
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
        if (a->result.i.lower() >= b->result.i.upper())
        {
            b->clearFlag(CLAUSE_FLAG_IGNORED);
        }
        else if (b->result.i.lower() >= a->result.i.upper())
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
