/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <unordered_map>

#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/tree/tree.hpp"
#include "libfive/tree/data.hpp"

namespace libfive {

Deck::Deck(const Tree& root_) {
    const auto root = root_.optimized();
    auto flat = root.walk();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<Tree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

    // Write the flattened tree into the tape!
    // It's reversed in this pass, then flipped when writing to tape->t below
    std::vector<Clause> rev;
    rev.reserve(flat.size());
    for (const auto& m : flat) {
        auto op = m->op();
        switch (op) {
            case Opcode::CONSTANT:
                constants.push_back({id, m->value()}); break;
            case Opcode::VAR_FREE:
                vars.left.insert({id, m}); break;
            case Opcode::ORACLE:
                rev.push_back({Opcode::ORACLE, id,
                    static_cast<unsigned int>(oracles.size()), 0});
                oracles.push_back(m->build_oracle());
                break;
            case Opcode::VAR_X:  // fallthrough
            case Opcode::VAR_Y:  // fallthrough
            case Opcode::VAR_Z:
                break;
            default:
                rev.push_back(
                    {op, id,
                     Opcode::args(op) >= 1 ? clauses.at(m->lhs().id()) : 0,
                     Opcode::args(op) >= 2 ? clauses.at(m->rhs().id()) : 0});
                break;
        }
        clauses[m] = id--;
    }
    assert(id == 0);

    tape.reset(new Tape);
    tape->type = Tape::BASE;
    tape->t.reserve(rev.size());
    for (auto itr = rev.rbegin(); itr != rev.rend(); ++itr) {
        tape->t.push_back(*itr);
    }

    // Make sure that X, Y, Z have been allocated space
    Tree axes[3] = {Tree::X(), Tree::Y(), Tree::Z()};
    for (const auto& a : axes) {
        if (clauses.find(a.id()) == clauses.end()) {
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

    // Add empty contexts for every oracle in the tape
    tape->contexts.resize(oracles.size());

    // Store the index of the tree's root
    assert(clauses.at(root.id()) == 1);
    tape->i = clauses.at(root.id());
}

void Deck::bindOracles(const Tape& tape)
{
    for (unsigned i=0; i < oracles.size(); ++i)
    {
        oracles[i]->bind(tape.getContext(i));
    }
}

void Deck::unbindOracles()
{
    for (auto& o : oracles)
    {
        o->unbind();
    }
}

}   // namespace libfive
