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

    // Store the active ranges of various variables
    std::unordered_map<Tree::Id, std::pair<unsigned, unsigned>> ranges;
    ranges[nullptr] = std::make_pair(0, 0);
    unsigned i=0;
    for (const auto& m : flat)
    {
        ranges[m.id()] = std::make_pair(i, i + 1);
        for (auto ptr : {m->lhs.get(), m->rhs.get()})
        {
            auto itr = ranges.find(ptr);
            assert(itr != ranges.end());
            itr->second.second = i;
        }
        i++;
    }

    // Construct a simple sorted tape of LOAD, DROP pairs (one per clause)
    enum RegOp { DROP, LOAD };
    std::multimap<std::pair<unsigned, RegOp>, Tree::Id> reg_ops;
    for (const auto& r : ranges)
    {
        reg_ops.insert({{r.second.first, LOAD}, r.first});
        reg_ops.insert({{r.second.second, DROP}, r.first});
    }

    // Walk the LOAD/DROP tape, assigning Trees to data slots
    std::map<Tree::Id, unsigned> active;
    std::map<Tree::Id, unsigned> assigned;
    std::set<unsigned> inactive;
    for (auto& r : reg_ops)
    {
        // Skip the dummy slot
        if (r.second == nullptr)
        {
            continue;
        }
        // Return the register to the free list
        if (r.first.second == DROP)
        {
            auto itr = active.find(r.second);
            assert(itr != active.end());
            inactive.insert(itr->second);
            active.erase(itr);
        }
        // Otherwise, assign a new register, expanding the number as needed
        else if (r.first.second == LOAD)
        {
            unsigned chosen;
            if (inactive.size())
            {
                chosen = *inactive.begin();
            }
            else
            {
                chosen = active.size();
            }
            active.insert({r.second, chosen});
            assigned.insert({r.second, chosen});
        }
    }
    assigned.insert({nullptr, 0}); // Dummy assignment

    // Write the flattened tree into the tape!
    std::list<Clause> tape_;
    for (const auto& m : flat)
    {
        // Normal clauses end up in the tape
        if (m->rank > 0)
        {
            tape_.push_front(
                    {m->op,
                     assigned.at(m.id()),
                     assigned.at(m->lhs.get()),
                     assigned.at(m->rhs.get())});
        }
        // For constants and variables, record their values so
        // that we can store those values in the result array
        else if (m->op == Opcode::CONSTANT)
        {
            tape_.push_front(
                    {m->op, assigned.at(m.id()),
                     static_cast<unsigned>(constants.size()), 0});
            constants.push_back(m->value);
        }
        else if (m->op == Opcode::VAR_FREE)
        {
            tape_.push_front(
                    {m->op, assigned.at(m.id()),
                     static_cast<unsigned>(vars.size()), 0});
            vars.push_back(m.id());
        }
        // For oracles, store their position in the oracles vector
        // as the LHS of the clause, so that we can find them during
        // tape evaluation.
        else if (m->op == Opcode::ORACLE) {
            assert(m->oracle);

            tape_.push_front({Opcode::ORACLE, assigned.at(m.id()),
                    static_cast<unsigned int>(oracles.size()), 0});
            oracles.push_back(m->oracle->getOracle());
        }
        else
        {
            assert(m->op == Opcode::VAR_X ||
                   m->op == Opcode::VAR_Y ||
                   m->op == Opcode::VAR_Z);
            tape_.push_front({m->op, assigned.at(m.id()), 0, 0});
        }
    }

    //  Move from the list tape to a more-compact vector tape
    tapes.push_back(Subtape());
    tape = tapes.begin();
    for (auto& t : tape_)
    {
        tape->t.push_back(t);
    }

    // Store the total number of clauses
    // Remember, evaluators need to allocate one more than this
    // amount of space, as the clause with id = 0 is a placeholder
    num_clauses = inactive.size() - 1;

    // Allocate enough memory for all the clauses
    disabled.resize(flat.size());
    remap.resize(flat.size());

    // Store the index of the tree's root
    tape->i = assigned.at(root.id());
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

Tape::Handle Tape::push(std::function<Keep(Opcode::Opcode, Clause::Id,
                                           Clause::Id, Clause::Id)> fn,
                        Type t, Region<3> r)
{
    // Special-case: if this is a dummy tape, then increment the
    // tape's depth and return immediately.
    if (tape->dummy)
    {
        tape->dummy++;
        return Handle(this);
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

    return Handle(this);
}

Tape::Handle Tape::getBase(const Eigen::Vector3f& p)
{
    auto prev_tape = tape;

    // Walk up the tape stack until we find an interval-type tape
    // that contains the given point, or we hit the start of the stack
    while (tape != tapes.begin())
    {
        if (tape->type == Tape::INTERVAL &&
            p.x() >= tape->X.lower() && p.x() <= tape->X.upper() &&
            p.y() >= tape->Y.lower() && p.y() <= tape->Y.upper() &&
            p.z() >= tape->Z.lower() && p.z() <= tape->Z.upper())
        {
            break;
        }
        else
        {
            tape--;
        }
    }

    return Handle(this, prev_tape);
}

////////////////////////////////////////////////////////////////////////////////

Tape::Handle::~Handle()
{
    switch (type)
    {
        case NONE: break;
        case BASE: assert(tape); tape->tape = prev; break;
        case PUSH: assert(tape); tape->pop(); break;
    }
}

Tape::Handle::Handle(Handle&& other)
    : tape(other.tape), type(other.type), prev(other.prev)
{
    other.type = NONE;
}

Tape::Handle& Tape::Handle::operator=(Handle&& other)
{
    tape = other.tape;
    type = other.type;
    prev = other.prev;

    other.type = NONE;
    return *this;
}

}   // namespace Kernel
