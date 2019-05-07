/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <unordered_map>

#include "libfive/eval/tape.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/render/brep/region.hpp"

namespace Kernel {

Clause::Id Tape::rwalk(RWalkFunction fn, bool& abort)
{
    auto itr = t.crbegin();
    while (itr != t.crend() && !abort)
    {
        fn(itr);
    }
    return i;
}

void Tape::walk(WalkFunction fn, bool& abort)
{
    auto itr = t.cbegin();
    while (itr != t.cend() && !abort)
    {
        fn(itr);
    }
}

std::shared_ptr<Tape> Tape::push(const std::shared_ptr<Tape>& tape, Deck& deck,
                                 KeepFunction fn, Type t)
{
    return push(tape, deck, fn, t, Region<3>());
}

std::shared_ptr<Tape> Tape::push(const std::shared_ptr<Tape>& tape, Deck& deck,
                                 KeepFunction fn, Type t, const Region<3>& r)
{
    // If this tape has no min/max clauses, then return it right away
    if (tape->terminal)
    {
        return tape;
    }

    // Since we'll be figuring out which clauses are disabled and
    // which should be remapped, we reset those arrays here
    std::fill(deck.disabled.begin(), deck.disabled.end(), true);
    std::fill(deck.remap.begin(), deck.remap.end(), 0);

    // Mark the root node as active
    deck.disabled[tape->i] = false;

    // We'll store a temporary vector of Oracle contexts here.
    //
    // By default, these contexts will be the same as the previous tape,
    // but we'll call push on each Oracle to see if we should refine it
    // any further.
    std::vector<std::shared_ptr<OracleContext>> contexts = tape->contexts;
    assert(contexts.size() == deck.oracles.size());

    bool terminal = true;
    bool changed = false;
    for (const auto& c : tape->t)
    {
        if (!deck.disabled[c.id])
        {
            switch (fn(c.op, c.id, c.a, c.b))
            {
                case KEEP_A:        deck.disabled[c.a] = false;
                                    deck.remap[c.id] = c.a;
                                    changed = true;
                                    break;
                case KEEP_B:        deck.disabled[c.b] = false;
                                    deck.remap[c.id] = c.b;
                                    changed = true;
                                    break;
                case KEEP_BOTH:     terminal = false; // fallthrough
                case KEEP_ALWAYS:   break;
            }

            if (deck.remap[c.id])
            {
                deck.disabled[c.id] = true;
            }
            // Oracle nodes are special-cased here.  They should always
            // return either KEEP_BOTH or KEEP_ALWAYS, but have no children
            // to disable (and c.a is a dummy index into the oracles[]
            // array, so we shouldn't mis-interpret it as a clause index).
            else if (c.op != Opcode::ORACLE)
            {
                deck.disabled[c.a] = false;
                deck.disabled[c.b] = false;
            }
            else if (c.op == Opcode::ORACLE)
            {
                // Get the previous context, then use it to store
                // a new context for the oracle, marking whether it
                // has changed.
                assert(c.a < tape->contexts.size());
                auto prev = tape->contexts[c.a];

                deck.oracles[c.a]->bind(prev);
                contexts[c.a] = deck.oracles[c.a]->push(t);
                deck.oracles[c.a]->unbind();

                changed |= (contexts[c.a] != prev);
                terminal &= (contexts[c.a].get() != nullptr) ?
                    contexts[c.a]->isTerminal()
                    : true;
            }
        }
    }


    if (!changed)
    {
        return tape;
    }

    std::shared_ptr<Tape> out;
    if (deck.spares.size())
    {
        out = deck.spares.back();
        deck.spares.pop_back();
    }
    else
    {
        out.reset(new Tape);
    }
    out->t.reserve(tape->t.size());

    out->type = t;
    out->parent = tape;
    out->terminal = terminal;
    out->t.clear(); // preserves capacity

    // Now, use the data in disabled and remap to make the new tape
    for (const auto& c : tape->t)
    {
        if (!deck.disabled[c.id])
        {
            // Oracle nodes use c.a as an index into tape->oracles,
            // rather than the address of an lhs / rhs expression,
            // so we special-case them here to avoid bad remapping.
            if (c.op == Opcode::ORACLE)
            {
                out->t.push_back({c.op, c.id, c.a, c.b});
            }
            else
            {
                Clause::Id ra, rb;
                for (ra = c.a; deck.remap[ra]; ra = deck.remap[ra]);
                for (rb = c.b; deck.remap[rb]; rb = deck.remap[rb]);
                out->t.push_back({c.op, c.id, ra, rb});
            }
        }
    }

    // Remap the tape root index
    for (out->i = tape->i; deck.remap[out->i]; out->i = deck.remap[out->i]);

    // Make sure that the tape got shorter
    assert(out->t.size() <= tape->t.size());

    // Store X / Y / Z bounds (may be irrelevant)
    out->X = {r.lower.x(), r.upper.x()};
    out->Y = {r.lower.y(), r.upper.y()};
    out->Z = {r.lower.z(), r.upper.z()};

    // Store the Oracle contexts
    out->contexts = std::move(contexts);

    return out;
}

std::shared_ptr<OracleContext> Tape::getContext(unsigned i) const
{
    assert(i < contexts.size());
    return contexts[i];
}

std::shared_ptr<Tape> Tape::getBase(std::shared_ptr<Tape> tape,
                                    const Region<3>& r)
{
    while (tape->parent.get()) {
        if (tape->type == Tape::INTERVAL &&
            r.lower.x() >= tape->X.lower() && r.upper.x() <= tape->X.upper() &&
            r.lower.y() >= tape->Y.lower() && r.upper.y() <= tape->Y.upper() &&
            r.lower.z() >= tape->Z.lower() && r.upper.z() <= tape->Z.upper())
        {
            break;
        }
        else
        {
            tape = tape->parent;
        }
    }

    return tape;
}

std::shared_ptr<Tape> Tape::getBase(
        std::shared_ptr<Tape> tape,
        const Eigen::Vector3f& p)
{
    // Walk up the tape stack until we find an interval-type tape
    // that contains the given point, or we hit the start of the stack
    while (tape->parent.get())
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
            tape = tape->parent;
        }
    }

    return tape;
}

}   // namespace Kernel
