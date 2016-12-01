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

#include "ao/kernel/tree/tree.hpp"

Tree::Tree(float v)
    : Tree(Cache::instance(), Cache::instance()->constant(v))
{
    // Nothing to do here
}

Tree::Tree(Opcode::Opcode op, Tree a, Tree b)
    : Tree(Cache::instance(), Cache::instance()->operation(op, a.id, b.id))
{
    assert(!a.id || a.parent == Cache::instance());
    assert(!b.id || b.parent == Cache::instance());

    // POW only accepts integral values as its second argument
    if (op == Opcode::POW)
    {
        assert(b.opcode() == Opcode::CONST &&
               b.value() == std::round(b.value()));
    }
    else if (op == Opcode::NTH_ROOT)
    {
        assert(b.opcode() == Opcode::CONST &&
               b.value() == std::round(b.value()) &&
               b.value() > 0);
    }
}

/*
 *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
 */
Tree Tree::affine(float a, float b, float c, float d)
{
    auto s = Cache::instance();
    return Tree(s, s->affine(a, b, c, d));
}

Tree Tree::var(float v)
{
    auto s = Cache::instance();
    return Tree(s, s->var(v));
}

Tree Tree::collapse() const
{
    return collapsed ? *this :
           Tree(parent, parent->collapse(id), true);
}

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode) \
Tree name(const Tree& a) { return Tree(opcode, a); }
OP_UNARY(square,    Opcode::SQUARE);
OP_UNARY(sqrt,      Opcode::SQRT);
Tree Tree::operator-() const { return Tree(Opcode::NEG, *this); }
OP_UNARY(abs,       Opcode::ABS);
OP_UNARY(sin,       Opcode::SIN);
OP_UNARY(cos,       Opcode::COS);
OP_UNARY(tan,       Opcode::TAN);
OP_UNARY(asin,      Opcode::ASIN);
OP_UNARY(acos,      Opcode::ACOS);
OP_UNARY(atan,      Opcode::ATAN);
OP_UNARY(exp,       Opcode::EXP);
#undef OP_UNARY

#define OP_BINARY(name, opcode) \
Tree name(const Tree& a, const Tree& b) { return Tree(opcode, a, b); }
OP_BINARY(operator+,    Opcode::ADD);
OP_BINARY(operator*,    Opcode::MUL);
OP_BINARY(min,          Opcode::MIN);
OP_BINARY(max,          Opcode::MAX);
OP_BINARY(operator-,    Opcode::SUB);
OP_BINARY(operator/,    Opcode::DIV);
OP_BINARY(atan2,        Opcode::ATAN2);
OP_BINARY(pow,          Opcode::POW);
OP_BINARY(nth_root,     Opcode::NTH_ROOT);
OP_BINARY(mod,          Opcode::MOD);
OP_BINARY(nanfill,      Opcode::NANFILL);
#undef OP_BINARY
