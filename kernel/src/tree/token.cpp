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

Token Token::constant(float v)
{
    auto s = new Cache();
    return Token(s->constant(v), s);
}

Token Token::operation(Opcode::Opcode op, Token a, Token b)
{
    Cache* t = nullptr;
    Cache::Id id_b = b.id;

    if (a.id)
    {
        // If the two Caches are different, then import b's store
        // into a and update the relevant id tag
        if (b.id && a.parent != b.parent)
        {
            id_b = a.parent->import(b.parent.get(), b.id);
        }
        t = a.parent.get();
    }
    else
    {
        t = new Cache();
    }
    assert(t);

    return Token(t->operation(op, a.id, id_b), t);
}

/*
 *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
 */
Token Token::affine(float a, float b, float c, float d)
{
    Cache* s = new Cache();
    return Token(s->affine(a, b, c, d), s);
}


std::tuple<Token,
           Token,
           Token> Token::axes()
{
    Cache* s = new Cache();
    return { Token(s->affine(1, 0, 0, 0), s),
             Token(s->affine(0, 1, 0, 0), s),
             Token(s->affine(0, 0, 1, 0), s) };
}

Token Token::X()
{
    return Token::operation(Opcode::VAR_X);
}

Token Token::Y()
{
    return Token::operation(Opcode::VAR_Y);
}

Token Token::Z()
{
    return Token::operation(Opcode::VAR_Z);
}

Token Token::collapse() const
{
    auto other = new Cache();
    return Token(other->collapse(other->import(parent.get(), id)), other);
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Accessor functions for Token data
 *  (which lives in the parent Cache)
 */
glm::vec4 Token::getAffine(bool* success)
    { return parent->getAffine(id, success); }
Opcode::Opcode Token::opcode() const
    { return parent->opcode(id); }
Token Token::lhs() const
    { return Token(parent->lhs(id), parent.get()); }
Token Token::rhs() const
    { return Token(parent->rhs(id), parent.get()); }
size_t Token::rank() const
    { return parent->rank(id); }
float Token::value() const
    { return parent->value(id); }
