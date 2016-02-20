/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <map>
#include <vector>
#include <array>

#include "ao/kernel/tree/opcode.hpp"

class Token;

/*
 *  A Store contains a set of Tokens with efficient routines for lookups
 */
class Store
{
public:
    /*
     *  In destructor, delete all Tokens associated with this Store
     */
    ~Store();

    /*
     *  Returns a token for the given constant
     */
    Token* constant(float v);

    /*
     *  Returns a token for the given operation
     *
     *  Arguments should be filled in from left to right
     *  (i.e. a must not be null if b is not null)
     */
    Token* operation(Opcode op, Token* a=nullptr, Token* b=nullptr);

    /*
     *  Return tokens for base variables
     */
    Token* X() { return operation(OP_X); }
    Token* Y() { return operation(OP_Y); }
    Token* Z() { return operation(OP_Z); }

    /*
     *  Set found in every token descending from root
     */
    void markFound(Token* root);

protected:
    /*
     *  Set the found member of each token to false
     */
    void clearFound();

    typedef std::pair<Token*, Token*> Key;
    typedef std::array<std::map<Key, Token*>, LAST_OP> Cache;

    /*  Constants are indexed solely by value  */
    std::map<float, Token*> constants;

    /*  Operators are indexed by weight, opcode, and arguments  */
    std::vector<Cache> ops;

    friend class Tree;
};
