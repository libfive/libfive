/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <unordered_map>

#include "libfive/eval/tape.hpp"

namespace Kernel {

Tape::Tape(const Tree root)
{
    auto flat = root.ordered();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<Tree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

    // Helper function to make a new function
    std::list<Clause> tape_;
    auto newClause = [&clauses, &id, &tape_](const Tree::Id t)
    {
        tape_.push_front(
                {t->op,
                 id,
                 clauses.at(t->lhs.get()),
                 clauses.at(t->rhs.get())});
    };

    // Write the flattened tree into the tape!
    for (const auto& m : flat)
    {
        // Normal clauses end up in the tape
        if (m->rank > 0)
        {
            newClause(m.id());
        }
        // For constants and variables, record their values so
        // that we can store those values in the result array
        else if (m->op == Opcode::CONSTANT)
        {
            constants[id] = m->value;
        }
        else if (m->op == Opcode::VAR_FREE)
        {
            vars.left.insert({id, m.id()});
        }
        // For oracles, store their position in the oracles vector
        // as the LHS of the clause, so that we can find them during
        // tape evaluation.
        else if (m->op == Opcode::ORACLE) {
            assert(m->oracle);

            tape_.push_front({Opcode::ORACLE, id,
                    static_cast<unsigned int>(oracles.size()), 0});
            oracles.push_back(m->oracle->getOracle());
        }
        else
        {
            assert(m->op == Opcode::VAR_X ||
                   m->op == Opcode::VAR_Y ||
                   m->op == Opcode::VAR_Z);
        }
        clauses[m.id()] = id--;
    }
    assert(id == 0);

    //  Move from the list tape to a more-compact vector tape
    tapes.push_back(Subtape());
    tape = tapes.begin();
    for (auto& t : tape_)
    {
        tape->t.push_back(t);
    }

    // Make sure that X, Y, Z have been allocated space
    std::vector<Tree> axes = {Tree::X(), Tree::Y(), Tree::Z()};
    for (auto a : axes)
    {
        if (clauses.find(a.id()) == clauses.end())
        {
            clauses.insert({a.id(), clauses.size()});
        }
    }

    // Store the total number of clauses
    // Remember, evaluators need to allocate one more than this
    // amount of space, as the clause with id = 0 is a placeholder
    num_clauses = clauses.size() - 1;

    // Allocate enough memory for all the clauses
    disabled.resize(clauses.size());
    remap.resize(clauses.size());

    // Save X, Y, Z ids
    X = clauses.at(axes[0].id());
    Y = clauses.at(axes[1].id());
    Z = clauses.at(axes[2].id());

    // Store the index of the tree's root
    assert(clauses.at(root.id()) == 1);
    tape->i = clauses.at(root.id());
};

void Tape::pop()
{
    assert(tape != tapes.begin());

    if (tape->dummy > 1)
    {
        tape->dummy--;
    }
    else
    {
        tape--;
    }
}

double Tape::utilization() const
{
    return tape->t.size() / double(tapes.front().t.size());
}

Clause::Id Tape::rwalk(std::function<void(Opcode::Opcode, Clause::Id,
                                          Clause::Id, Clause::Id)> fn,
                       bool& abort)
{
    for (auto itr = tape->t.rbegin(); itr != tape->t.rend() && !abort; ++itr)
    {
        fn(itr->op, itr->id, itr->a, itr->b);
    }
    return tape->i;
}

void Tape::walk(std::function<void(Opcode::Opcode, Clause::Id,
                                   Clause::Id, Clause::Id)> fn, bool& abort)
{
    for (auto itr = tape->t.begin(); itr != tape->t.end() && !abort; ++itr)
    {
        fn(itr->op, itr->id, itr->a, itr->b);
    }
}

void Tape::push(std::function<Keep(Opcode::Opcode, Clause::Id,
                                   Clause::Id, Clause::Id)> fn,
                Type t, Region<3> r)
{
    // Special-case: if this is a dummy tape, then increment the
    // tape's depth and return immediately.
    if (tape->dummy)
    {
        tape->dummy++;
        return;
    }

    // Since we'll be figuring out which clauses are disabled and
    // which should be remapped, we reset those arrays here
    std::fill(disabled.begin(), disabled.end(), true);
    std::fill(remap.begin(), remap.end(), 0);

    // Mark the root node as active
    disabled[tape->i] = false;
    bool has_choices = false;

    for (const auto& c : tape->t)
    {
        if (!disabled[c.id])
        {
            switch (fn(c.op, c.id, c.a, c.b))
            {
                case KEEP_A:        disabled[c.a] = false;
                                    remap[c.id] = c.a;
                                    break;
                case KEEP_B:        disabled[c.b] = false;
                                    remap[c.id] = c.b;
                                    break;
                case KEEP_BOTH:     has_choices = true;
                                    break;
                case KEEP_ALWAYS:   break;
            }

            if (remap[c.id])
            {
                disabled[c.id] = true;
            }
            // Oracle nodes are special-cased here.  They should always
            // return either KEEP_BOTH or KEEP_ALWAYS, but have no children
            // to disable (and c.a is a dummy index into the oracles[]
            // array, so we shouldn't mis-interpret it as a clause index).
            else if (c.op != Opcode::ORACLE)
            {
                disabled[c.a] = false;
                disabled[c.b] = false;
            }
        }
    }

    auto prev_tape = tape;

    // Add another tape to the top of the tape stack if one doesn't already
    // exist (we never erase them, to avoid re-allocating memory during
    // nested evaluations).
    if (++tape == tapes.end())
    {
        tape = tapes.insert(tape, Subtape());
        tape->t.reserve(tapes.front().t.size());
    }
    else
    {
        // We may be reusing an existing tape, so resize to 0
        // (preserving allocated storage)
        tape->t.clear();
    }

    assert(tape != tapes.end());
    assert(tape != tapes.begin());
    assert(tape->t.capacity() >= prev_tape->t.size());

    // Reset tape type
    tape->type = t;
    tape->dummy = has_choices ? 0 : 1;

    // Now, use the data in disabled and remap to make the new tape
    for (const auto& c : prev_tape->t)
    {
        if (!disabled[c.id])
        {
            // Oracle nodes use c.a as an index into tape->oracles,
            // rather than the address of an lhs / rhs expression,
            // so we special-case them here to avoid bad remapping.
            if (c.op == Opcode::ORACLE)
            {
                tape->t.push_back({c.op, c.id, c.a, c.b});
            }
            else
            {
                Clause::Id ra, rb;
                for (ra = c.a; remap[ra]; ra = remap[ra]);
                for (rb = c.b; remap[rb]; rb = remap[rb]);
                tape->t.push_back({c.op, c.id, ra, rb});
            }
        }
    }

    // Remap the tape root index
    for (tape->i = prev_tape->i; remap[tape->i]; tape->i = remap[tape->i]);

    // Make sure that the tape got shorter
    assert(tape->t.size() <= prev_tape->t.size());

    // Store X / Y / Z bounds (may be irrelevant)
    tape->X = {r.lower.x(), r.upper.x()};
    tape->Y = {r.lower.y(), r.upper.y()};
    tape->Z = {r.lower.z(), r.upper.z()};
}

}   // namespace Kernel
