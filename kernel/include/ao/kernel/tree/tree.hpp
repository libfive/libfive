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
     */
    Tree(Opcode::Opcode op, Tree a=Tree(nullptr, 0), Tree b=Tree(nullptr, 0));

    /*
     *  Constructors for individual axes (non-affine)
     *
     *  The three-axis constructor axes() should be preferred to using
     *  an individual-axis constructor, as it ensures that they're backed
     *  by the same Cache object
     */
    static Tree X() { return Tree(Opcode::VAR_X); }
    static Tree Y() { return Tree(Opcode::VAR_Y); }
    static Tree Z() { return Tree(Opcode::VAR_Z); }

    /*
     *  Returns an AFFINE token (of the form a*x + b*y + c*z + d)
     */
    static Tree affine(float a, float b, float c, float d);

    /*
     *  Returns a tuple of affine tokens for X, Y, Z axes
     */
    static std::tuple<Tree, Tree, Tree> axes();

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

protected:
    /*
     *  Private constructor
     *  (only ever called by Cache)
     */
    explicit Tree(std::shared_ptr<Cache> parent, Cache::Id id=0) : parent(parent), id(id) {}

    /*  Shared pointer to parent Cache
     *  Every token that refers back to this cache has a pointer to it,
     *  and the Cache is only deleted when all tokens are destroyed     */
    std::shared_ptr<Cache> parent;

    /*  ID indexing into the parent Cache */
    const Cache::Id id;

    /*  An Evaluator needs to be able to pull out the parent Cache */
    friend class Evaluator;
};

