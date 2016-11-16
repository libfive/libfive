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
    : parent(Cache::instance()), id(parent->constant(v))
{
    // Nothing to do here
}

Tree::Tree(Opcode::Opcode op, Tree a, Tree b)
    : parent(Cache::instance()), id(parent->operation(op, a.id, b.id))
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

Tree Tree::collapse() const
{
    return Tree(parent, parent->collapse(id));
}
