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
    std::fill(deck.n_ary_keep.begin(), deck.n_ary_keep.end(), false);

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
            switch (fn(c, n_ary_data.data(), deck.n_ary_keep.data()))
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
            else if (c.op == Opcode::OP_NARY_MIN || c.op == Opcode::OP_NARY_MAX)
            {
                unsigned keep_count = 0;
                Clause::Id remap_to = 0;
                for (unsigned i=c.a; i < c.b; ++i) {
                    if (deck.n_ary_keep[i]) {
                        keep_count++;
                        remap_to = n_ary_data[i];
                        deck.disabled[n_ary_data[i]] = false;
                    } else {
                        // We've disabled something!
                        changed = true;
                    }
                }
                assert(keep_count >= 1);

                // If only one clause was active, then remap to it
                if (keep_count == 1) {
                    assert(remap_to);
                    deck.remap[c.id] = remap_to;
                    deck.disabled[c.id] = true;
                } else {
                    deck.disabled[c.id] = false;
                }
            }
            // Other n-ary operations (that don't store any data in
            // n_ary_keep) leave all of their children enabled.
            else if (Opcode::isNary(c.op))
            {
                for (unsigned i=c.a; i != c.b; ++i) {
                    deck.disabled[n_ary_data[i]] = false;
                    deck.n_ary_keep[i] = true;
                }
            }
            else
            {
                deck.disabled[c.a] = false;
                deck.disabled[c.b] = false;
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
    out->n_ary_data.reserve(n_ary_data.size());

    out->type = type;
    out->parent = shared_from_this();
    out->terminal = terminal;

    // Clear any previous data (preserving capacity)
    out->t.clear();
    out->n_ary_data.clear();

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
            else if (Opcode::isNary(c.op))
            {
                const unsigned start = out->n_ary_data.size();
                for (unsigned i=c.a; i != c.b; ++i) {
                    if (deck.n_ary_keep[i]) {
                        Clause::Id r;
                        for (r = n_ary_data[i];
                             deck.remap[r];
                             r = deck.remap[r]);
                        out->n_ary_data.push_back(r);
                    }
                }
                const unsigned end = out->n_ary_data.size();

                // Collapse to standard min if there are only two
                // n-ary nodes remaining
                if (end - start == 2) {
                    // n-ary nodes must have more than two arguments, and
                    // the only ones which should shorten are min and max
                    // (since the others can't prune branches).
                    assert(c.op == Opcode::OP_NARY_MIN ||
                           c.op == Opcode::OP_NARY_MAX);
                    out->t.push_back({
                            Opcode::fromNary(c.op), c.id,
                            out->n_ary_data[end - 2],
                            out->n_ary_data[end - 1]});
                    out->n_ary_data.pop_back();
                    out->n_ary_data.pop_back();
                } else {
                    assert(end - start > 2);
                    out->t.push_back({c.op, c.id, start, end});
                }
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
