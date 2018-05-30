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
    ranges.insert({nullptr, std::make_pair(0, 0)});
    unsigned i=0;
    for (const auto& m : flat)
    {
        ranges.insert({m.id(), std::make_pair(i, i + 1)});
        for (auto ptr : {m->lhs.get(), m->rhs.get()})
        {
            auto itr = ranges.find(ptr);
            assert(itr != ranges.end());
            itr->second.second = i + 1;
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
        else if (r.first.second == DROP)
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
                auto itr = inactive.begin();
                chosen = *itr;
                inactive.erase(itr);
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

    // Store the total number of memory slots
    num_clauses = inactive.size();

    // Allocate enough memory for all the slots
    disabled.resize(num_clauses);

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
    assert(tape->t.size() > 0);
    return tape->t.begin()->id;
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

    // Mark the root node as active
    assert(tape->t.size() > 0);
    disabled[tape->t.begin()->id] = false;
    bool has_choices = false;

    // Add another tape to the top of the tape stack if one doesn't already
    // exist (we never erase them, to avoid re-allocating memory during
    // nested evaluations).
    auto prev_tape = tape;
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

    // Store X / Y / Z bounds (may be irrelevant)
    tape->X = {r.lower.x(), r.upper.x()};
    tape->Y = {r.lower.y(), r.upper.y()};
    tape->Z = {r.lower.z(), r.upper.z()};

    for (auto itr=prev_tape->t.begin(); itr != prev_tape->t.end(); ++itr)
    {
        if (!disabled[itr->id])
        {
            // This id may be reused later, so mark it as disabled now
            disabled[itr->id] = true;
            switch (fn(itr->op, itr->id, itr->a, itr->b))
            {
                case KEEP_A:        disabled[itr->a] = false;
                                    remap(tape->t.rbegin(), itr->id, itr->a);
                                    break;
                case KEEP_B:        disabled[itr->b] = false;
                                    remap(tape->t.rbegin(), itr->id, itr->b);
                                    break;
                case KEEP_BOTH:     has_choices = true; // fallthrough
                case KEEP_ALWAYS:   if (!hasDummyChildren(itr->op))
                                    {
                                        disabled[itr->a] = false;
                                        disabled[itr->b] = false;
                                    }
                                    tape->t.push_back(*itr);
                                    break;
            }
        }
    }

    // Make sure that the tape got shorter
    assert(tape->t.size() <= prev_tape->t.size());

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

bool Tape::hasDummyChildren(Opcode::Opcode op)
{
    return (op == Opcode::CONSTANT)
        || (op == Opcode::VAR_FREE)
        || (op == Opcode::ORACLE);
}

void Tape::remap(std::vector<Clause>::reverse_iterator rev,
                 Clause::Id id, Clause::Id alt)
{
    auto next = tape->t.rend();
    Clause::Id next_id, next_alt;

    while (rev != tape->t.rend())
    {
        if (rev->a == id)   rev->a = alt;
        if (rev->b == id)   rev->b = alt;

        if (rev->id == alt) {
            next = rev + 1;
            next_id = rev->id;
            next_alt = id;
            rev->id = id;
        }

        // If we've reached a node that re-uses the remapped id,
        // then we can stop remapping (because the meaning of that
        // slot changes from here on out).
        if (rev->id != id) break;
        rev++;
    }
    if (next != tape->t.rend())
    {
        remap(next, next_id, next_alt);
    }
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
