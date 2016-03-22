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

#define CLAUSE_FLAG_IGNORED  1
#define CLAUSE_FLAG_DISABLED 2

/*
 *  A clause is used in an Evaluator to evaluate a tree
 */
class Clause
{
public:
    explicit Clause(const Atom* m,
                    std::unordered_map<const Atom*, Clause*>& clauses);

    /*
     *  Flag manipulation functions
     */
    void setFlag(uint8_t f)     { flags |=  f; }
    void clearFlag(uint8_t f)   { flags &= ~f; }
    void clearFlags()           { flags  =  0; }

    /*
     *  If the CLAUSE_FLAG_IGNORED flag is set:
     *      Clears the flag
     *      Returns true.
     *
     *  Otherwise, propagates the IGNORED flag to its children,
     *  properly handling min and max operations (which may only
     *  leave one child active), returning false.
     */
    bool checkDisabled();

    /*
     *  Sets or clears CLAUSE_FLAG_DISABLED
     */
    void disable() {   setFlag(CLAUSE_FLAG_DISABLED); }
    void enable()  { clearFlag(CLAUSE_FLAG_DISABLED); }

protected:
    /*  Opcode for this clause  */
    const Opcode op;

    /*  Populated for OP_CONST clause */
    const float value;

    /*  Flags are set during evaluation for various purposes  */
    uint8_t flags=0;

    /*  Populated for operators with arguments */
    Clause* const a;
    Clause* const b;

    /*  Store pointers to various result arrays for fast evaluation
     *  (since a and b don't change, no need to check them for null on
     *   every iteration through an evaluation)*/
    struct ResultPtrs { float *f, *dx, *dy, *dz; };
    struct { ResultPtrs a;
             ResultPtrs b; } ptrs;

    /*  Results are stored in a struct */
    Result result;

    friend class Evaluator;
};
