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

namespace libfive {

Tape::Handle Tape::push(Deck& deck, KeepFunction fn, Type t)
{
    return push(deck, fn, t, Region<3>());
}

Tape::Handle Tape::push(Deck& deck, KeepFunction fn, Type type,
                        const Region<3>& r)
{
    // If this tape has no min/max clauses, then return it right away
    if (terminal)
    {
        return shared_from_this();
    }

    // Since we'll be figuring out which clauses are disabled and
    // which should be remapped, we reset those arrays here
    std::fill(deck.disabled.begin(), deck.disabled.end(), true);
    std::fill(deck.remap.begin(), deck.remap.end(), 0);

    // Mark the root node as active
    deck.disabled[i] = false;

    // We'll store a temporary vector of Oracle contexts here.
    //
    // By default, these contexts will be the same as the previous tape,
    // but we'll call push on each Oracle to see if we should refine it
    // any further.
    std::vector<std::shared_ptr<OracleContext>> new_contexts = contexts;
    assert(new_contexts.size() == deck.oracles.size());

    bool terminal = true;
    bool changed = false;
    for (const auto& c : t)
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
                assert(c.a < contexts.size());
                auto prev = contexts[c.a];

                deck.oracles[c.a]->bind(prev);
                new_contexts[c.a] = deck.oracles[c.a]->push(type);
                deck.oracles[c.a]->unbind();

                changed |= (new_contexts[c.a] != prev);
                terminal &= (new_contexts[c.a].get() == nullptr) ||
                             new_contexts[c.a]->isTerminal();
            }
        }
    }


    if (!changed)
    {
        return shared_from_this();
    }

    Tape::Handle out;
    if (deck.spares.size())
    {
        out = deck.spares.back();
        deck.spares.pop_back();
    }
    else
    {
        out.reset(new Tape);
    }
    out->t.reserve(t.size());

    out->type = type;
    out->parent = shared_from_this();
    out->terminal = terminal;
    out->t.clear(); // preserves capacity

    // Now, use the data in disabled and remap to make the new tape
    for (const auto& c : t)
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
    for (out->i = i; deck.remap[out->i]; out->i = deck.remap[out->i]);

    // Make sure that the tape got shorter
    assert(out->t.size() <= t.size());

    // Store X / Y / Z bounds (may be irrelevant)
    out->X = {r.lower.x(), r.upper.x()};
    out->Y = {r.lower.y(), r.upper.y()};
    out->Z = {r.lower.z(), r.upper.z()};

    // Store the Oracle contexts
    out->contexts = std::move(new_contexts);

    return out;
}

std::shared_ptr<OracleContext> Tape::getContext(unsigned i) const
{
    assert(i < contexts.size());
    return contexts[i];
}

Tape::Handle Tape::getBase(const Region<3>& r)
{
    auto tape = shared_from_this();
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

Tape::Handle Tape::getBase(const Eigen::Vector3f& p)
{
    // Walk up the tape stack until we find an interval-type tape
    // that contains the given point, or we hit the start of the stack
    auto tape = shared_from_this();
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

}   // namespace libfive
