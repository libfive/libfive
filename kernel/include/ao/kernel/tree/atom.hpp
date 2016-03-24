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

#include <vector>
#include <iostream>
#include <unordered_map>

#include "ao/kernel/tree/opcode.hpp"

class Token;

/*
 *  An Atom represent an operation in a math tree.
 */
class Atom
{
public:
    /*
     *  Construct an atom from the given token, storing atoms[t] = this
     *
     *  Requires that the token's children be packed into Atoms
     *  beforehand (otherwise will throw an assertion)
     */
    explicit Atom(const Token* t,
                  std::unordered_map<const Token*, Atom*>& atoms);

    /*
     *  Print an Atom to an ostream
     */
    friend std::ostream& operator<<(std::ostream& os, const Atom& atom);

protected:
    /*  Opcode for this atom  */
    const Opcode op;

    /*  Populated for OP_CONST atoms */
    const float value;

    /*  Populated for operators with arguments */
    Atom* const a;
    Atom* const b;

    friend class Tree;
    friend class Clause;
    friend class Evaluator;
};

std::ostream& operator<<(std::ostream& os, const Atom& atom);
