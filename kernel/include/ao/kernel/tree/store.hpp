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
#include <set>
#include <boost/bimap.hpp>

#include "glm/vec4.hpp"

#include "ao/kernel/tree/opcode.hpp"
#include "ao/kernel/tree/token.hpp"

/*
 *  A Store contains a set of Tokens with efficient routines for lookups
 */
class Store
{
public:
    /*
     *  Returns a token for the given constant
     */
    Token::Id constant(float v);

    /*
     *  Returns a token for the given operation
     *
     *  Arguments should be filled in from left to right
     *  (i.e. a must not be null if b is not null)
     *
     *  If collapse is true (the default), identity and affine operations will
     *  be collapsed; if false, all branches will be created
     */
    Token::Id operation(Opcode::Opcode op, Token::Id a=0, Token::Id b=0,
                        bool collapse=true);

    Token::Id X() { return operation(Opcode::VAR_X); }
    Token::Id Y() { return operation(Opcode::VAR_Y); }
    Token::Id Z() { return operation(Opcode::VAR_Z); }

    /*
     *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
     */
    Token::Id affine(float a, float b, float c, float d);
    Token::Id affine(glm::vec4 v) { return affine(v.x, v.y, v.z, v.w); }

    /*
     *  Set found in every token descending from root
     */
    std::set<Token::Id> findConnected(Token::Id root);

    /*
     *  If the given Token is an AFFINE_VEC, return the affine terms
     *
     *  Set success to true / false if it is provided
     */
    glm::vec4 getAffine(Token::Id root, bool* success=nullptr) const;

    /*
     *  Collapses BOUNDS nodes into normal OP_MAX, taking advantage of
     *  identity operations to make the tree smaller.  Returns the new root
     *  token (which may have changed).
     *
     *  Invalidates all Token pointers.
     */
    Token::Id collapseBounds(Token::Id root);

    /*
     *  Collapses AFFINE nodes into normal OP_ADD, taking advantage of
     *  identity operations to make the tree smaller.  Returns the new root
     *  token (which may have changed).
     *
     *  Invalidates all Token pointers.
     */
    Token::Id collapseAffine(Token::Id root);

    /*
     *  Imports an external Store, returning the new root's id
     */
    Token::Id import(Store* s, Token::Id root);

    /*
     *  Accessor functions for token fields
     */
    Opcode::Opcode opcode(Token::Id id) const
        { return std::get<0>(token(id)); }
    Token::Id lhs(Token::Id id) const
        { return std::get<1>(token(id)); }
    Token::Id rhs(Token::Id id) const
        { return std::get<2>(token(id)); }
    size_t rank(Token::Id id) const
        { return std::get<3>(token(id)); }
    float value(Token::Id id) const
        { return std::get<4>(token(id)); }

protected:
    /*
     *  Checks whether the operation is an identity operation
     *  If so returns an appropriately simplified Token
     *  i.e. (X + 0) will return X
     */
    Token::Id checkIdentity(Opcode::Opcode op, Token::Id a, Token::Id b);

    /*
     *  Checks whether the operation should be handled as an affine
     *  transformation, returning an AFFINE Token if true.
     */
    Token::Id checkAffine(Opcode::Opcode op, Token::Id a, Token::Id b);

    /*
     *  Rebuilds a tree from the base up, returning the new root
     */
    Token::Id rebuild(Token::Id root, std::set<Token::Id> pruned,
                      std::map<Token::Id, Token::Id> changed);

    /*
     *  Keys store all relevant token data
     */
    typedef std::tuple<Opcode::Opcode, Token::Id, Token::Id, size_t, float> Key;

    /*
     *  Key constructors
     */
    Key key(float v) const;
    Key key(Opcode::Opcode op, Token::Id a, Token::Id b) const;

    /*
     *  Token reverse lookup
     */
    Key token(Token::Id id) const { return cache.right.at(id); }

    boost::bimap<Key, Token::Id> cache;
    Token::Id next=1;

    friend class Tree;
};
