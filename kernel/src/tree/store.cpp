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

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/token.hpp"

Token::Id Store::constant(float v)
{
    auto k = key(v);
    if (cache.left.find(k) == cache.left.end())
    {
        cache.left[k] = next++;
    }
    return cache.left[k];
}

Token::Id Store::operation(Opcode::Opcode op, Token::Id a, Token::Id b,
                           bool collapse)
{
    // These are opcodes that you're not allowed to use here
    assert(op != CONST && op != INVALID &&
           op != DUMMY_A && op != DUMMY_B && op != LAST_OP);

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
        cache.left[k] = next++;
    }

    return cache.left[k];
}

Token::Id Store::affine(float a, float b, float c, float d)
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

Token::Id Store::checkAffine(Opcode::Opcode op, Token::Id a, Token::Id b)
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
            auto sb = value(a);
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

Token::Id Store::checkIdentity(Opcode op, Token* a, Token* b)
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

glm::vec4 Store::getAffine(Token::Id root, bool* success) const
{
    if (op != AFFINE_VEC)
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

std::set<Token*> Store::findConnected(Token* root)
{
    std::set<Token*> found = {root};

    // Iterate over weight levels from top to bottom
    for (auto weight = ops.rbegin(); weight != ops.rend(); ++weight)
    {
        // Iterate over operations in a given weight level
        for (auto op : *weight)
        {
            // Iterate over Key, Token* pairs
            for (auto c : op)
            {
                Token* const t = c.second;
                if (found.find(t) != found.end())
                {
                    if (t->a)
                    {
                        found.insert(t->a);
                    }
                    if (t->b)
                    {
                        found.insert(t->b);
                    }
                }
            }
        }
    }

    return found;
}

Token* Store::rebuild(Token* root, std::set<Token*> pruned,
                      std::map<Token*, Token*> changed)
{
    // Deep copy of ops so that changes don't invalidate iterators
    decltype(ops) ops_ = ops;

    // Iterate over weight levels from bottom to top
    for (const auto& weight : ops_)
    {
        // Iterate over operations in a given weight level
        for (const auto& op : weight)
        {
            // Iterate over Key, Token* pairs
            for (auto itr = op.begin(); itr != op.end(); ++itr)
            {
                // Get child pointers
                auto a = itr->second->a;
                auto b = itr->second->b;

                // If either of the child pointers has changed, regenerate
                // the operation to ensure correct pointers and weight
                if (changed.count(a) || changed.count(b))
                {
                    changed[itr->second] = operation(itr->second->op,
                        changed.count(a) ? changed[a] : a,
                        changed.count(b) ? changed[b] : b);

                    pruned.insert(itr->second);
                }
            }
        }
    }

    // Then, we'll remove pruned tokens from the actual ops cache
    for (auto& weight : ops)
    {
        // Iterate over operations in a given weight level
        for (auto& op : weight)
        {
            // Iterate over Key, Token* pairs
            for (auto itr = op.begin(); itr != op.end();)
            {
                if (pruned.count(itr->second))
                {
                    itr = op.erase(itr);
                }
                else
                {
                    ++itr;
                }
            }
        }
    }

    return changed.count(root) ? changed[root] : root;
}

Token* Store::collapseAffine(Token* root)
{
    // If the tree isn't big enough to have any affine functions,
    // then we can safely return right away
    if (ops.size() < 4)
    {
        return root;
    }

    // Deep copy of affine clauses so that changes don't invalidate iterators
    auto afs = ops[3][AFFINE_VEC];

    // These are tokens that should be removed from the tree
    std::set<Token*> pruned;

    // Turn every AFFINE into a normal OP_ADD
    // (with identity operations automatically cancelled out)
    std::map<Token*, Token*> changed;
    for (auto r : afs)
    {
        changed[r.second] = operation(OP_ADD,
                operation(OP_ADD,
                    operation(OP_MUL, X(), r.second->a->a->b),
                    operation(OP_MUL, Y(), r.second->a->b->b)),
                operation(OP_ADD,
                    operation(OP_MUL, Z(), r.second->b->a->b),
                    r.second->b->b));

        pruned.insert(r.second);
    }

    return rebuild(root, pruned, changed);
}
