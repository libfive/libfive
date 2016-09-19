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
#include <algorithm>
#include <cmath>
#include <cassert>

#include "ao/kernel/tree/token.hpp"
#include "ao/kernel/tree/store.hpp"

Token* Token::constant(float v)
{
    auto s = new Store();
    return new Token(s->constant(v), s);
}

Token* Token::operation(Opcode::Opcode op, Token* a, Token* b)
{
    Store* s = nullptr;
    Id a_id = a ? a->id : 0;
    Id b_id = b ? b->id : 0;

    if (a && b)
    {
        // If the two Stores are different, then import b's store
        // into a and update the relevant id tag
        if (a->parent != b->parent)
        {
            b_id = a->parent->import(b->parent.get(), b->id);
        }
        s = a->parent.get();
    }
    else if (a)
    {
        s = a->parent.get();
    }

    if (!s)
    {
        s = new Store();
    }
    return new Token(s->operation(op, a_id, b_id), s);
}

/*
 *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
 */
Token* Token::affine(float a, float b, float c, float d)
{
    Store* s = new Store();
    return new Token(s->affine(a, b, c, d), s);
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Accessor functions for Token data
 *  (which lives in the parent Store)
 */
glm::vec4 Token::getAffine(bool* success)
    { return parent->getAffine(id, success); }
Opcode::Opcode Token::opcode() const
    { return parent->opcode(id); }
Token::Id Token::lhs() const
    { return parent->lhs(id); }
Token::Id Token::rhs() const
    { return parent->rhs(id); }
size_t Token::rank() const
    { return parent->rank(id); }
float Token::value() const
    { return parent->value(id); }
