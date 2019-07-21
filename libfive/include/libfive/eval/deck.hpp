/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <boost/bimap.hpp>

#include "libfive/tree/tree.hpp"
#include "libfive/eval/clause.hpp"
#include "libfive/oracle/oracle.hpp"

namespace libfive {

class Tape; /* Foward declaration */

/*
 *  A Deck is the top-level class that produces Tapes.  It includes
 *  meta-data like the number of clauses, constant values, and variable,
 *  mapping.
 *
 *  When evaluating, you should have one Deck per thread, because
 *  Oracles are stored on a per-Deck basis.
 *
 *  The deck contains a list of constants and variables which are used
 *  to construct Evaluators, and pointers to Oracles that are used during
 *  evaluation.
 */
class Deck
{
public:
    Deck(const Tree root);

    Deck(const Deck&)=delete;
    Deck& operator=(const Deck& other)=delete;

    /*  Indices of X, Y, Z coordinates */
    Clause::Id X, Y, Z;

    /*  Constants, unpacked from the tree at construction */
    std::map<Clause::Id, float> constants;

    /*  Map of variables (in terms of where they live in this Evaluator) to
     *  their ids in their respective Tree (e.g. what you get when calling
     *  Tree::var().id() */
    boost::bimap<Clause::Id, Tree::Id> vars;

    /*  Oracles are also unpacked from the tree at construction, and
     *  stored in this flat list.  The ORACLE opcode takes an index into
     *  this list and an index into the results array. */
    std::vector<std::unique_ptr<Oracle>> oracles;

    /*  Stores the total number of clauses (including X/Y/Z, oracles,
     *  variables, and constants, which aren't explicitly in the tape).
     *  This is used by Evaluators to decide how many memory slots to allocate
     *  for results during Tape evaluation. */
    size_t num_clauses;

    /*  This is the top-level tape associated with this Deck. */
    std::shared_ptr<Tape> tape;

    /*  Moves this tape into the spares bin, so it can be reused later */
    void claim(std::shared_ptr<Tape>&& tape) {
        spares.push_back(tape);
    }

    /*
     *  Binds all oracles to the contexts in the given tape
     */
    void bindOracles(const Tape& tape);

    /*
     *  Unbinds all oracles, setting their contexts to null
     */
    void unbindOracles();

protected:
    /*  Temporary storage, used when pushing into a Tape  */
    std::vector<uint8_t> disabled;
    std::vector<Clause::Id> remap;

    /*  We can keep spare tapes around, to avoid reallocating their data */
    std::vector<std::shared_ptr<Tape>> spares;

    friend class Tape;
};

} // namespace libfive
