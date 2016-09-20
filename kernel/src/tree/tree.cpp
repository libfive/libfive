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
#include <list>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/token.hpp"

/******************************************************************************
 * Token constructors
 ******************************************************************************/
Token::Id Tree::constant(float v)
{
    auto k = key(v);
    if (cache.left.find(k) == cache.left.end())
    {
        cache.insert({k, next});
    }
    return cache.left.at(k);
}

Token::Id Tree::operation(Opcode::Opcode op, Token::Id a, Token::Id b,
                          bool collapse)
{
    // These are opcodes that you're not allowed to use here
    assert(op != Opcode::CONST &&
           op != Opcode::INVALID &&
           op != Opcode::DUMMY_A &&
           op != Opcode::DUMMY_B &&
           op != Opcode::LAST_OP);

    // See if we can simplify the expression, either because it's an identity
    // operation (e.g. X + 0) or a linear combination of affine forms
    if (collapse)
    {
        if (auto t = checkIdentity(op, a, b))
        {
            return t;
        }
        else if (auto t = checkAffine(op, a, b))
        {
            return t;
        }
    }

    // Otherwise, construct a new Token and add it to the ops set
    auto k = key(op, a, b);
    if (cache.left.find(k) == cache.left.end())
    {
        cache.insert({k, next++});
    }

    return cache.left.at(k);
}

Token::Id Tree::affine(float a, float b, float c, float d)
{
    // Build up the desired tree structure with collapse = false
    // to keep branches from automatically collapsing.
    return operation(Opcode::AFFINE_VEC,
            operation(Opcode::OP_ADD,
                operation(Opcode::OP_MUL, X(), constant(a), false),
                operation(Opcode::OP_MUL, Y(), constant(b), false), false),
            operation(Opcode::OP_ADD,
                operation(Opcode::OP_MUL, Z(), constant(c), false),
                constant(d), false));
}

/******************************************************************************
 * Clause simplification
 ******************************************************************************/
Token::Id Tree::checkAffine(Opcode::Opcode op, Token::Id a, Token::Id b)
{
    if (Opcode::args(op) != 2)
    {
        return 0;
    }

    // Pull op-codes from both branches
    auto op_a = opcode(a);
    auto op_b = opcode(b);

    if (op == Opcode::OP_ADD)
    {
        if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::CONST)
        {
            auto va = getAffine(a);
            auto vb = glm::vec4(0, 0, 0, value(b));
            return affine(va + vb);
        }
        else if (op_b == Opcode::AFFINE_VEC && op_a == Opcode::CONST)
        {
            auto va = glm::vec4(0, 0, 0, value(a));
            auto vb = getAffine(b);
            return affine(va + vb);
        }
        else if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::AFFINE_VEC)
        {
            auto va = getAffine(b);
            auto vb = getAffine(b);
            return affine(va + vb);
        }
    }
    else if (op == Opcode::OP_SUB)
    {
        if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::CONST)
        {
            auto va = getAffine(a);
            auto vb = glm::vec4(0, 0, 0, value(b));
            return affine(va - vb);
        }
        else if (op_b == Opcode::AFFINE_VEC && op_a == Opcode::CONST)
        {
            auto va = glm::vec4(0, 0, 0, value(a));
            auto vb = getAffine(b);
            return affine(va - vb);
        }
        else if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::AFFINE_VEC)
        {
            auto va = getAffine(b);
            auto vb = getAffine(b);
            return affine(va - vb);
        }
    }
    else if (op == Opcode::OP_MUL)
    {
        if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::CONST)
        {
            auto va = getAffine(a);
            auto sb = value(b);
            return affine(va * sb);
        }
        else if (op_b == Opcode::AFFINE_VEC && op_a == Opcode::CONST)
        {
            auto sa = value(a);
            auto vb = getAffine(b);
            return affine(vb * sa);
        }
    }
    else if (op == Opcode::OP_DIV)
    {
        if (op_a == Opcode::AFFINE_VEC && op_b == Opcode::CONST)
        {
            auto va = getAffine(a);
            auto sb = value(b);
            return affine(va / sb);
        }
    }
    return 0;
}

Token::Id Tree::checkIdentity(Opcode::Opcode op, Token::Id a, Token::Id b)
{
    if (Opcode::args(op) != 2)
    {
        return 0;
    }

    // Pull op-codes from both branches
    auto op_a = opcode(a);
    auto op_b = opcode(b);

    // Special cases to handle identity operations
    if (op == Opcode::OP_ADD)
    {
        if (op_a == Opcode::CONST && value(a) == 0)
        {
            return b;
        }
        else if (op_b == Opcode::CONST && value(b) == 0)
        {
            return a;
        }
    }
    else if (op == Opcode::OP_SUB)
    {
        if (op_a == Opcode::CONST && value(a) == 0)
        {
            return operation(Opcode::OP_NEG, b);
        }
        else if (op_b == Opcode::CONST && value(b) == 0)
        {
            return a;
        }
    }
    else if (op == Opcode::OP_MUL)
    {
        if (op_a == Opcode::CONST)
        {
            if (value(a) == 0)
            {
                return a;
            }
            else if (value(a) == 1)
            {
                return b;
            }
        }
        if (op_b == Opcode::CONST)
        {
            if (value(b) == 0)
            {
                return b;
            }
            else if (value(b) == 1)
            {
                return a;
            }
        }
    }
    return 0;
}

/******************************************************************************
 * Utilities
 ******************************************************************************/
glm::vec4 Tree::getAffine(Token::Id root, bool* success) const
{
    if (opcode(root) != Opcode::AFFINE_VEC)
    {
        if (success != nullptr)
        {
            *success = false;
        }
        return {};
    }

    if (success != nullptr)
    {
        *success = true;
    }

    return {value(rhs(lhs(lhs(root)))), value(rhs(rhs(lhs(root)))),
            value(rhs(lhs(rhs(root)))), value(rhs(rhs(root)))};
}

/******************************************************************************
 * Key constructors
 ******************************************************************************/
Tree::Key Tree::key(float v) const
{
    return Key(v);
}

Tree::Key Tree::key(Opcode::Opcode op, Token::Id a, Token::Id b) const
{
    return Key(op, a, b, std::max(a ? rank(a) + 1 : 0,
                                  b ? rank(b) + 1 : 0));
}

/******************************************************************************
 * Tree walking and modification
 ******************************************************************************/
Token::Id Tree::import(Tree* other, Token::Id root)
{
    if (other == this)
    {
        return root;
    }

    std::map<Token::Id, Token::Id> changed;
    for (auto t : other->walk())
    {
        if (other->opcode(t) == Opcode::CONST)
        {
            // Import the constant into the tree
            changed[t] = constant(other->value(t));
        }
        else
        {
            // Get child pointers
            auto a = other->lhs(t);
            auto b = other->rhs(t);

            // Then import the operation into the tree
            changed[t] = operation(other->opcode(t),
                changed.count(a) ? changed[a] : a,
                changed.count(b) ? changed[b] : b);
        }
    }

    return changed[root];
}

std::vector<Token::Id> Tree::walk() const
{
    std::multimap<size_t, Token::Id> sorted;

    // Find nodes ranked on a per-level basis
    for (auto c : cache.left)
    {
        sorted.insert({c.first.rank(), c.second});
    }

    // Then sort into a flat array
    std::vector<Token::Id> out;
    for (auto v : sorted)
    {
        out.push_back(v.first);
    }

    return out;
}

std::set<Token::Id> Tree::findConnected(Token::Id root)
{
    std::set<Token::Id> found = {root};
    auto tokens = walk();

    // Iterate over weight levels from top to bottom
    for (auto t = tokens.rbegin(); t != tokens.rend(); ++t)
    {
        if (found.find(*t) != found.end())
        {
            if (Token::Id a = lhs(*t))
            {
                found.insert(a);
            }
            if (Token::Id b = rhs(*t))
            {
                found.insert(b);
            }
        }
    }

    return found;
}

Token::Id Tree::rebuild(Token::Id root, std::set<Token::Id> pruned,
                        std::map<Token::Id, Token::Id> changed)
{
    auto tokens = walk();

    // Iterate over weight levels from bottom to top
    for (auto t : tokens)
    {
        // Get child pointers
        auto a = lhs(t);
        auto b = rhs(t);

        // If either of the child pointers has changed, regenerate
        // the operation to ensure correct pointers and weight
        if (changed.count(a) || changed.count(b))
        {
            changed[t] = operation(opcode(t),
                changed.count(a) ? changed[a] : a,
                changed.count(b) ? changed[b] : b);

            pruned.insert(t);
        }
    }

    // Then, we'll remove pruned tokens from the actual ops cache
    for (auto c : pruned)
    {
        cache.right.erase(c);
    }

    return changed.count(root) ? changed[root] : root;
}

Token::Id Tree::collapseAffine(Token::Id root)
{
    // Deep copy of clauses so that changes don't invalidate iterators
    auto tokens = walk();

    // Details on which nodes have changed
    std::set<Token::Id> pruned;
    std::map<Token::Id, Token::Id> changed;

    // Turn every AFFINE into a normal OP_ADD
    // (with identity operations automatically cancelled out)
    for (auto t : tokens)
    {
        if (opcode(t) == Opcode::AFFINE_VEC)
        {
            auto v = getAffine(t);
            changed[t] = operation(Opcode::OP_ADD,
                    operation(Opcode::OP_ADD,
                        operation(Opcode::OP_MUL, X(), v.x),
                        operation(Opcode::OP_MUL, Y(), v.y)),
                    operation(Opcode::OP_ADD,
                        operation(Opcode::OP_MUL, Z(), v.z),
                        v.w));
        }

        pruned.insert(t);
    }

    return rebuild(root, pruned, changed);
}
