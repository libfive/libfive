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

Tree Tree::constant(float v)
{
    auto s = new Cache();
    return Tree(s->constant(v), s);
}

Tree Tree::operation(Opcode::Opcode op, Tree a, Tree b)
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

    return Tree(t->operation(op, a.id, id_b), t);
}

/*
 *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
 */
Tree Tree::affine(float a, float b, float c, float d)
{
    Cache* s = new Cache();
    return Tree(s->affine(a, b, c, d), s);
}


std::tuple<Tree, Tree, Tree> Tree::axes()
{
    Cache* s = new Cache();
    return { Tree(s->affine(1, 0, 0, 0), s),
             Tree(s->affine(0, 1, 0, 0), s),
             Tree(s->affine(0, 0, 1, 0), s) };
}

Tree Tree::X()
{
    return Tree::operation(Opcode::VAR_X);
}

Tree Tree::Y()
{
    return Tree::operation(Opcode::VAR_Y);
}

Tree Tree::Z()
{
    return Tree::operation(Opcode::VAR_Z);
}

Tree Tree::collapse() const
{
    auto other = new Cache();
    return Tree(other->collapse(other->import(parent.get(), id)), other);
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
    { return Tree(parent->lhs(id), parent.get()); }
Tree Tree::rhs() const
    { return Tree(parent->rhs(id), parent.get()); }
size_t Tree::rank() const
    { return parent->rank(id); }
float Tree::value() const
    { return parent->value(id); }
