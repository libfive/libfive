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
    : parent(new Cache()), id(parent->constant(v))
{
    // Nothing to do here
}

Tree::Tree(Opcode::Opcode op, Tree a, Tree b)
    : parent(a.id ? a.parent.get() : new Cache()),
      id(parent->operation(op, a.id, parent->import(b.parent.get(), b.id)))
{
    // Nothing to do here
}

/*
 *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
 */
Tree Tree::affine(float a, float b, float c, float d)
{
    Cache* s = new Cache();
    return Tree(s, s->affine(a, b, c, d));
}


std::tuple<Tree, Tree, Tree> Tree::axes()
{
    Cache* s = new Cache();
    return { Tree(s, s->affine(1, 0, 0, 0)),
             Tree(s, s->affine(0, 1, 0, 0)),
             Tree(s, s->affine(0, 0, 1, 0)) };
}

Tree Tree::X()
{
    return Tree(Opcode::VAR_X);
}

Tree Tree::Y()
{
    return Tree(Opcode::VAR_Y);
}

Tree Tree::Z()
{
    return Tree(Opcode::VAR_Z);
}

Tree Tree::collapse() const
{
    auto other = new Cache();
    return Tree(other, other->collapse(other->import(parent.get(), id)));
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Accessor functions for Tree data
 *  (which lives in the parent Cache)
 */
glm::vec4 Tree::getAffine(bool* success)
    { return parent->getAffine(id, success); }
Opcode::Opcode Tree::opcode() const
    { return parent->opcode(id); }
Tree Tree::lhs() const
    { return Tree(parent.get(), parent->lhs(id)); }
Tree Tree::rhs() const
    { return Tree(parent.get(), parent->rhs(id)); }
size_t Tree::rank() const
    { return parent->rank(id); }
float Tree::value() const
    { return parent->value(id); }
