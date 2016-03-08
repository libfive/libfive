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

#include <cstdlib>

#include "ao/kernel/tree/opcode.hpp"

/*
 *  A token represents a single expression (with up to two arguments)
 */
class Token
{
public:
    /*
     *  Constructs a token for an operation
     */
    explicit Token(Opcode op, Token* a=nullptr, Token* b=nullptr);

    /*
     *  Constructs a token for a constant
     */
    explicit Token(float v);

    /*
     *  Returns the number of arguments for the given token
     */
    static size_t args(Opcode op);

    /*
     *  Returns the found flag
     */
    bool isFound() const { return found; }

    /*  Member variables  */
    const Opcode op;
    const size_t weight;

    /*  If this token is a constant, value is populated  */
    const float value;

    /*  Otherwise, pointers to arguments are stored in a and b  */
    Token* const a;
    Token* const b;

protected:
    /*  found is used to detect which tokens are in the tree  */
    bool found=false;

    friend class Store;
    friend class Tree;
    friend class Atom;
};
