/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <unordered_map>

#include "ao/kernel/tree/opcode.hpp"
#include "ao/kernel/eval/result.hpp"

class Atom;

/*
 *  A clause is used in an Evaluator to evaluate a tree
 */
class Clause
{
public:
    /*
     *  Clause constructor
     *
     *  The clauses map is used to look up children, which must already have
     *  been turned into clauses (i.e. the tree must be built from the lowest
     *  levels up).
     */
    explicit Clause(const Atom* m,
                    std::unordered_map<const Atom*, Clause*>& clauses);

    /*
     *  If the disabled flag is set, returns true
     *
     *  Otherwise, clears the disabled flag in its children, handling min
     *  and max operations (which may only leave one child active),
     *  returning false.
     */
    bool checkDisabled();

    /*
     *  Sets or clears CLAUSE_FLAG_DISABLED
     */
    void disable() { disabled = true; }
    void enable()  { disabled = false; }

protected:
    /*  Opcode for this clause  */
    const Opcode::Opcode op;

    /*  Populated for CONST clause */
    const float value;

    /*  Flag used in tree pruning  */
    bool disabled = false;

    /*  Populated for operators with arguments */
    Clause* const a;
    Clause* const b;

    /*  Store pointers to various result arrays for fast evaluation
     *  (since a and b don't change, no need to check them for null on
     *   every iteration through an evaluation)*/
#ifdef __AVX__
    struct ResultPtrs { float *f, *dx, *dy, *dz;
                        __m256 *mf, *mdx, *mdy, *mdz; };
#else
    struct ResultPtrs { float *f, *dx, *dy, *dz; };
#endif
    struct { ResultPtrs a;
             ResultPtrs b; } ptrs;

    /*  Results are stored in a struct */
    Result result;

    friend class Evaluator;
};
