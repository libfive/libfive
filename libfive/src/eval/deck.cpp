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
#include "libfive/tree/simple_tree.hpp"

namespace libfive {

Deck::Deck(const Tree root)
{
    auto flat = root.ordered();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<Tree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

    // Helper function to make a new function
    std::list<Clause> tape_;
    auto newClause = [&clauses, &id, &tape_](const Tree& t)
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
            newClause(m);
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
    tape.reset(new Tape);
    tape->type = Tape::BASE;
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

    // Add empty contexts for every oracle in the tape
    tape->contexts.resize(oracles.size());

    // Store the index of the tree's root
    assert(clauses.at(root.id()) == 1);
    tape->i = clauses.at(root.id());
}

Deck::Deck(const SimpleUniqueTree& root) {
    auto flat = root.tree.walk();

    // Helper function to create a new clause in the data array
    // The dummy clause (0) is mapped to the first result slot
    std::unordered_map<SimpleTree::Id, Clause::Id> clauses = {{nullptr, 0}};
    Clause::Id id = flat.size();

    // Write the flattened tree into the tape!
    // It's reversed in this pass, then flipped when writing to tape->t below
    std::vector<Clause> rev;
    rev.reserve(flat.size());
    for (const auto& m : flat) {
        auto op = m->op();
        switch (op) {
            case Opcode::CONSTANT:
                constants[id] = m->value(); break;
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
    SimpleTree axes[3] = {SimpleTree::X(), SimpleTree::Y(), SimpleTree::Z()};
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
    assert(clauses.at(root.tree.id()) == 1);
    tape->i = clauses.at(root.tree.id());
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
