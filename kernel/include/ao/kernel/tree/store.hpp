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

#include <map>
#include <vector>
#include <array>
#include <set>

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
     *  Returns an AFFINE_ROOT of the form a*x + b*y + c*z + d
     */
    Token* affine(float a, float b, float c, float d);

    /*
     *  Set found in every token descending from root
     */
    std::set<Token*> findConnected(Token* root);

protected:
    /*
     *  Checks whether the operation is an identity operation
     *  If so returns an appropriately simplified Token
     *  i.e. (X + 0) will return X
     */
    Token* checkIdentity(Opcode op, Token* a, Token* b);

    /*
     *  Checks whether the operation should be handled as an affine
     *  transformation, returning an AFFINE_ROOT Token if true.
     */
    Token* checkAffine(Opcode op, Token* a, Token* b);

    typedef std::pair<Token*, Token*> Key;
    typedef std::array<std::map<Key, Token*>, LAST_OP> Cache;

    /*  Constants are indexed solely by value  */
    std::map<float, Token*> constants;

    /*  Affine values are stored in a single vector.
     *  They can't be combined like constants, because each AFFINE_ROOT tree
     *  needs to be independent - cross-linking would corrupt matrix stuff  */
    std::vector<Token*> affine_values;

    /*  Affine values are indexed by vec4, as each one represents
     *  a term of the form a*x + b*y + c*z + d.  AFFINE_ROOTS are also stored
     *  in the operations array, but that array's deduplication isn't useful
     *  (since it only looks at children, which will be unique)  */
    std::map<std::tuple<float, float, float, float>, Token*> affine_roots;

    /*  Operators are indexed by weight, opcode, and arguments  */
    std::vector<Cache> ops;

    friend class Tree;
};
