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
     *
     *  If collapse is true (the default), identity and affine operations will
     *  be collapsed; if false, all branches will be created
     */
    Token* operation(Opcode op, Token* a=nullptr, Token* b=nullptr,
                     bool collapse=true);

    /*
     *  Return tokens for base variables
     */
    Token* X() { return operation(OP_X); }
    Token* Y() { return operation(OP_Y); }
    Token* Z() { return operation(OP_Z); }

    /*
     *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
     */
    Token* affine(float a, float b, float c, float d);

    /*
     *  Set found in every token descending from root
     */
    std::set<Token*> findConnected(Token* root);

    /*
     *  Collapses AFFINE nodes into normal OP_ADD, taking advantage of
     *  identity operations to make the tree smaller.  Returns the new root
     *  token (which may have changed).
     *
     *  Invalidates all Token pointers.
     */
    Token* collapseAffine(Token* root);

protected:
    /*
     *  Checks whether the operation is an identity operation
     *  If so returns an appropriately simplified Token
     *  i.e. (X + 0) will return X
     */
    Token* checkIdentity(Opcode op, Token* a, Token* b);

    /*
     *  Checks whether the operation should be handled as an affine
     *  transformation, returning an AFFINE Token if true.
     */
    Token* checkAffine(Opcode op, Token* a, Token* b);

    typedef std::pair<Token*, Token*> Key;
    typedef std::array<std::map<Key, Token*>, LAST_OP> Cache;

    /*  Constants are indexed solely by value  */
    std::map<float, Token*> constants;

    /*  Operators are indexed by weight, opcode, and arguments  */
    std::vector<Cache> ops;

    friend class Tree;
};
