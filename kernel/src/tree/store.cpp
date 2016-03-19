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

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/token.hpp"

Store::~Store()
{
    for (auto c : constants)
    {
        delete c.second;
    }

    for (auto v : affine_values)
    {
        delete v;
    }

    for (auto row : ops)
    {
        for (auto sub : row)
        {
            for (auto t : sub)
            {
                delete t.second;
            }
        }
    }
}

Token* Store::constant(float v)
{
    if (constants.find(v) == constants.end())
    {
        constants[v] = new Token(v);
    }
    return constants[v];
}

Token* Store::checkAffine(Opcode op, Token* a, Token* b)
{
    if (op == OP_ADD)
    {
        if (a->op == AFFINE_ROOT && b->op == OP_CONST)
        {
            return affine(a->a->a->b->value,
                          a->a->b->b->value,
                          a->b->a->b->value,
                          a->b->b->value + b->value);
        }
        else if (b->op == AFFINE_ROOT && a->op == OP_CONST)
        {
            return affine(b->a->a->b->value,
                          b->a->b->b->value,
                          b->b->a->b->value,
                          b->b->b->value + a->value);
        }
        else if (a->op == AFFINE_ROOT && b->op == AFFINE_ROOT)
        {
            return affine(a->a->a->b->value + b->a->a->b->value,
                          a->a->b->b->value + b->a->b->b->value,
                          a->b->a->b->value + b->b->a->b->value,
                          a->b->b->value + b->b->b->value);
        }
    }
    else if (op == OP_SUB)
    {
        if (a->op == AFFINE_ROOT && b->op == OP_CONST)
        {
            return affine(a->a->a->b->value,
                          a->a->b->b->value,
                          a->b->a->b->value,
                          a->b->b->value - b->value);
        }
        else if (b->op == AFFINE_ROOT && a->op == OP_CONST)
        {
            return affine(-b->a->a->b->value,
                          -b->a->b->b->value,
                          -b->b->a->b->value,
                          a->value - b->b->b->value);
        }
        else if (a->op == AFFINE_ROOT && b->op == AFFINE_ROOT)
        {
            return affine(a->a->a->b->value - b->a->a->b->value,
                          a->a->b->b->value - b->a->b->b->value,
                          a->b->a->b->value - b->b->a->b->value,
                          a->b->b->value - b->b->b->value);
        }
    }
    else if (op == OP_MUL)
    {
        if (a->op == AFFINE_ROOT && b->op == OP_CONST)
        {
            return affine(a->a->a->b->value * b->value,
                          a->a->b->b->value * b->value,
                          a->b->a->b->value * b->value,
                          a->b->b->value * b->value);
        }
        else if (b->op == AFFINE_ROOT && a->op == OP_CONST)
        {
            return affine(b->a->a->b->value * a->value,
                          b->a->b->b->value * a->value,
                          b->b->a->b->value * a->value,
                          b->b->b->value * a->value);
        }
    }
    else if (op == OP_DIV)
    {
        if (a->op == AFFINE_ROOT && b->op == OP_CONST)
        {
            return affine(a->a->a->b->value / b->value,
                          a->a->b->b->value / b->value,
                          a->b->a->b->value / b->value,
                          a->b->b->value / b->value);
        }
    }
    return nullptr;
}

Token* Store::checkIdentity(Opcode op, Token* a, Token* b)
{
    // Special cases to handle identity operations
    if (op == OP_ADD)
    {
        if (a->op == OP_CONST && a->value == 0)
        {
            return b;
        }
        else if (b->op == OP_CONST && b->value == 0)
        {
            return a;
        }
    }
    else if (op == OP_SUB)
    {
        if (a->op == OP_CONST && a->value == 0)
        {
            return operation(OP_NEG, b);
        }
        else if (b->op == OP_CONST && b->value == 0)
        {
            return a;
        }
    }
    else if (op == OP_MUL)
    {
        if (a->op == OP_CONST)
        {
            if (a->value == 0)
            {
                return a;
            }
            else if (a->value == 1)
            {
                return b;
            }
        }
        if (b->op == OP_CONST)
        {
            if (b->value == 0)
            {
                return b;
            }
            else if (b->value == 1)
            {
                return a;
            }
        }
    }
    return nullptr;
}

Token* Store::operation(Opcode op, Token* a, Token* b)
{
    // These are opcodes that you're not allowed to use here
    assert(op != AFFINE_VALUE && op != OP_CONST && op != INVALID &&
           op != OP_A && op != OP_B && op != LAST_OP);

    // See if we can simplify the expression, either because it's an identity
    // operation (e.g. X + 0) or a linear combination of affine forms
    if (auto t = checkIdentity(op, a, b))
    {
        return t;
    }
    else if (auto t = checkAffine(op, a, b))
    {
        return t;
    }

    // Otherwise, construct a new Token and add it to the ops set
    const auto t = new Token(op, a, b);

    if (ops.size() <= t->weight)
    {
        ops.resize(t->weight + 1);
    }

    auto& row = ops[t->weight][t->op];
    if (row.find({a,b}) == row.end())
    {
        row[{a,b}] = t;
    }
    else
    {
        delete t;
    }

    return row[{a,b}];
}

Token* Store::affine(float a, float b, float c, float d)
{
    if (affine_roots.find({a, b, c, d}) == affine_roots.end())
    {
        auto a_ = new Token(a, AFFINE_VALUE);
        auto b_ = new Token(b, AFFINE_VALUE);
        auto c_ = new Token(c, AFFINE_VALUE);
        auto d_ = new Token(d, AFFINE_VALUE);

        for (auto t : {a_, b_, c_, d_})
        {
            affine_values.push_back(t);
        }

        affine_roots[{a,b,c,d}] = operation(AFFINE_ROOT,
                    operation(OP_ADD,
                        operation(OP_MUL, X(), a_),
                        operation(OP_MUL, Y(), b_)),
                    operation(OP_ADD,
                        operation(OP_MUL, Z(), c_), d_));
    }
    return affine_roots[{a,b,c,d}];
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

Token* Store::collapseAffine(Token* root)
{
    std::map<Token*, Token*> changed;

    // Turn every AFFINE_ROOT into a normal OP_ADD
    // (with identity operations automatically cancelled out)
    for (auto r : affine_roots)
    {
        changed[r.second] = operation(OP_ADD,
                operation(OP_ADD,
                    operation(OP_MUL, X(), constant(std::get<0>(r.first))),
                    operation(OP_MUL, Y(), constant(std::get<1>(r.first)))),
                operation(OP_ADD,
                    operation(OP_MUL, Z(), constant(std::get<2>(r.first))),
                    constant(std::get<3>(r.first))));
    }

    // Iterate over weight levels from bottom to top
    for (auto& weight : ops)
    {
        // Iterate over operations in a given weight level
        for (auto& op : weight)
        {
            // Iterate over Key, Token* pairs
            for (auto itr = op.begin(); itr != op.end();)
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

                    // Delete the Token and remove it from the Cache
                    delete itr->second;
                    itr = op.erase(itr);
                }
                else
                {
                    itr++;
                }
            }
        }
    }

    return changed.count(root) ? changed[root] : root;
}
