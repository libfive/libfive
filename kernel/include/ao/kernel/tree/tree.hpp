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
#include <memory>

#include "glm/vec4.hpp"

#include "ao/kernel/tree/opcode.hpp"
#include "ao/kernel/tree/cache.hpp"

/*
 *  A Tree represents a tree of math expressions.
 *
 *  Trees are implemented as a flyweight with a shared pointer to a
 *  Cache that stores deduplicated tree data  (children, values, etc).
 */
class Tree
{
public:
    /*
     *  Returns a Tree for the given constant
     */
    Tree(float v);

    /*
     *  Returns a token for the given operation
     *
     *  Arguments should be filled in from left to right
     *  (i.e. a must not be null if b is not null)
     *
     *  If the opcode is POW or NTH_ROOT, b must be an integral CONST
     *  (otherwise an assertion will be triggered).
     *  If the opcode is NTH_ROOT, b must be > 0.
     */
    explicit Tree(Opcode::Opcode op, Tree a=Tree(nullptr, 0, false),
                                     Tree b=Tree(nullptr, 0, false));

    /*
     *  Constructors for individual axes (non-affine)
     */
    static Tree X() { return Tree(Opcode::VAR_X); }
    static Tree Y() { return Tree(Opcode::VAR_Y); }
    static Tree Z() { return Tree(Opcode::VAR_Z); }

    /*
     *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
     */
    static Tree affine(float a, float b, float c, float d);

    /*
     *  Returns a new variable with the given value
     */
    static Tree var(float v);

    /*
     *  Returns a new Tree that is a flattened copy of this tree
     */
    Tree collapse() const;

    /*
     *  Attempts to get affine terms from a AFFINE_VEC token
     *  If success is provided, it is populated with true or false
     */
    glm::vec4 getAffine(bool* success=nullptr)
        { return parent->getAffine(id, success); }

    /*
     *  Accessors for token fields
     */
    Opcode::Opcode opcode() const   { return parent->opcode(id); }
    Tree lhs() const                { return Tree(parent, parent->lhs(id)); }
    Tree rhs() const                { return Tree(parent, parent->rhs(id)); }
    size_t rank() const             { return parent->rank(id); }
    float value() const             { return parent->value(id); }
    size_t var() const              { return parent->lhs(id); }

    /*
     *  Overloaded operators
     */
    Tree operator-() const;

protected:
    /*
     *  Private constructor
     */
    explicit Tree(std::shared_ptr<Cache> parent, Cache::Id id=0,
                  bool collapsed=false)
        : parent(parent), id(id), collapsed(collapsed) {}

    /*  Shared pointer to parent Cache
     *  Every token that refers back to this cache has a pointer to it,
     *  and the Cache is only deleted when all tokens are destroyed     */
    std::shared_ptr<Cache> parent;

    /*  ID indexing into the parent Cache */
    const Cache::Id id;

    /*  Is this tree pre-collapsed and suitable for rendering?  */
    const bool collapsed;

    /*  An Evaluator needs to be able to pull out the parent Cache */
    friend class Evaluator;
};

// Mass-produce declarations for overloaded operations
#define OP_UNARY(name)      Tree name(const Tree& a)
OP_UNARY(square);
OP_UNARY(sqrt);
OP_UNARY(abs);
OP_UNARY(sin);
OP_UNARY(cos);
OP_UNARY(tan);
OP_UNARY(asin);
OP_UNARY(acos);
OP_UNARY(atan);
OP_UNARY(exp);
#undef OP_UNARY

#define OP_BINARY(name)     Tree name(const Tree& a, const Tree& b)
OP_BINARY(operator+);
OP_BINARY(operator*);
OP_BINARY(min);
OP_BINARY(max);
OP_BINARY(operator-);
OP_BINARY(operator/);
OP_BINARY(atan2);
OP_BINARY(pow);
OP_BINARY(nth_root);
OP_BINARY(mod);
OP_BINARY(nanfill);
#undef OP_BINARY
