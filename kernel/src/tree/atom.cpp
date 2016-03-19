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
#include <cassert>

#include "ao/kernel/tree/atom.hpp"
#include "ao/kernel/tree/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Atom::Atom(const Token* t, std::unordered_map<const Token*, Atom*>& atoms)
    : op(t->op), value(t->value),
      a(t->a ? atoms[t->a] : nullptr),
      b(t->b ? atoms[t->b] : nullptr)
{
    // Assert that children have atom pointers populated
    assert(t->a ? atoms.count(t->a) : true);
    assert(t->b ? atoms.count(t->b) : true);

    // Assert that this token hasn't already been added to the tree
    assert(atoms[t] == nullptr);

    atoms[t] = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(std::nan("")), a(a), b(b)
{
    // Nothing to do here
}

////////////////////////////////////////////////////////////////////////////////

std::ostream& operator<<(std::ostream& os, const Atom& atom)
{
    switch (atom.op)
    {
        case OP_ADD:
        case AFFINE_ROOT:   os << "(" << *atom.a << " + " << *atom.b << ")"; break;
        case OP_MUL:    os << "(" << *atom.a << " * " << *atom.b << ")"; break;
        case OP_MIN:    os << "min(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_MAX:    os << "max(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_SUB:    os << "(" << *atom.a << " - " << *atom.b << ")"; break;
        case OP_DIV:    os << "(" << *atom.a << " / " << *atom.b << ")"; break;
        case OP_ATAN2:  os << "atan2(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_MOD:    os << "mod(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_NANFILL:    os << "nanfill(" << *atom.a << ", " << *atom.b << ")"; break;

        case OP_SQUARE: os << "square(" << *atom.a << ")"; break;
        case OP_SQRT:   os << "sqrt(" << *atom.a << ")"; break;
        case OP_NEG:    os << "(-" << *atom.a << ")"; break;
        case OP_ABS:    os << "abs(" << *atom.a << ")"; break;
        case OP_SIN:    os << "sin(" << *atom.a << ")"; break;
        case OP_COS:    os << "cos(" << *atom.a << ")"; break;
        case OP_TAN:    os << "tan(" << *atom.a << ")"; break;
        case OP_ASIN:    os << "asin(" << *atom.a << ")"; break;
        case OP_ACOS:    os << "acos(" << *atom.a << ")"; break;
        case OP_ATAN:    os << "atan(" << *atom.a << ")"; break;
        case OP_EXP:    os << "exp(" << *atom.a << ")"; break;

        case OP_CONST:
        case AFFINE_VALUE:  os << atom.value; break;
        case OP_X:      os << "X"; break;
        case OP_Y:      os << "Y"; break;
        case OP_Z:      os << "Z"; break;

        case LAST_OP:   // Fallthrough!
        case OP_A:
        case OP_B:
        case INVALID:   assert(false);
    }
    return os;
}
