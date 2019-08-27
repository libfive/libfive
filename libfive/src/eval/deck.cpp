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
    auto newClause = [&clauses, &id, &tape_](const Tree::Id t)
    {
        tape_.push_front(
                {t->op,
                 id,
                 clauses.at(t->lhs.get()),
                 clauses.at(t->rhs.get())});
    };

    // In order to expand min operations into n-ary min clauses, we
    // first need to find cases where there's a min that only has one
    // parent, which is also a min.
    //
    // We build two maps:
    //      n_ary_parents is a MIN parent of any MIN node
    //      num_parents is the number of parents of a MIN node
    //
    // Iff a MIN node has only one parent and which is also a MIN, it can be
    // collapsed into an NARY_MIN node.
    std::map<Tree::Id, Tree::Id> n_ary_parents;
    std::map<Tree::Id, uint32_t> num_parents;
    for (auto itr=flat.crbegin(); itr != flat.crend(); ++itr) {
        if ((*itr)->op == Opcode::OP_MIN) {
            for (auto& p: {(*itr)->lhs, (*itr)->rhs}) {
                if (p->op == Opcode::OP_MIN) {
                    n_ary_parents[p.get()] = itr->id();
                }
            }
        }
        // Count all parents, even those that aren't MINs, e.g. in the tree
        //      min(x, min(y, z)) + 2 + min(y, z)
        // we'll need to make min(y, z) a separate node for the addition op
        switch (Opcode::args((*itr)->op)) {
            case 2:
                if ((*itr)->rhs->op == Opcode::OP_MIN) {
                    num_parents[(*itr)->rhs.get()]++;
                }   // FALLTHROUGH
            case 1:
                if ((*itr)->lhs->op == Opcode::OP_MIN) {
                    num_parents[(*itr)->lhs.get()]++;
                }   // FALLTHROUGH (doesn't matter)
            default:
                break;
        }
    }
    {   // Having built those two maps, we'll now go through and filter out
        // the nodes in n_ary_parents that have more than one parent
        auto itr = n_ary_parents.begin();
        while (itr != n_ary_parents.end()) {
            assert(num_parents.contains(itr->first));
            if (num_parents[itr->first] > 1) {
                itr = n_ary_parents.erase(itr);
            } else {
                itr++;
            }
        }
    }
    // Next, we go through and adjust every item in n_ary_parents to point to
    // the top-most parent, which is what it will be collected under.
    for (auto& k: n_ary_parents) {
        for (auto itr=n_ary_parents.find(k.second);
             itr != n_ary_parents.end();
             n_ary_parents.find(itr->second))
        {
            k.second = itr->first;
        }
    }
    // Reverse the map, so that we have a map of each parent to one or
    // more children.
    std::map<Tree::Id, std::set<Tree::Id>> n_ary_children;
    for (auto& k: n_ary_parents) {
        n_ary_children[k.second].insert(k.first);
    }
    // Finally, we skip any n-ary operations which only have two children
    // (since that's less efficient than a single opcode), and record all
    // of the children which should be skipped
    std::set<Tree::Id> n_ary_skip;
    {
        auto itr = n_ary_children.begin();
        while (itr != n_ary_children.end()) {
            if (itr->second.size() > 2) {
                for (auto& c : itr->second) {
                    n_ary_skip.insert(c);
                }
                itr++;
            } else {
                itr = n_ary_children.erase(itr);
            }
        }
    }

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
