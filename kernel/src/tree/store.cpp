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
#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/token.hpp"

Store::~Store()
{
    for (auto c : constants)
    {
        delete c.second;
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

Token* Store::operation(Opcode op, Token* a, Token* b)
{
    // Special cases to handle identity operations
    if (op == OP_ADD)
    {
        if (a->op == OP_CONST && a->value == 0)
        {
            return b;
        }
        if (b->op == OP_CONST && b->value == 0)
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
        if (b->op == OP_CONST && b->value == 0)
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
    return operation(AFFINE_ROOT,
                operation(OP_ADD,
                    operation(OP_MUL, X(), constant(a)),
                    operation(OP_MUL, Y(), constant(b))),
                operation(OP_ADD,
                    operation(OP_MUL, Z(), constant(c)), constant(d)));
}

void Store::clearFound()
{
    for (auto a : ops)
    {
        for (auto b : a)
        {
            for (auto c : b)
            {
                c.second->found = false;
            }
        }
    }
}

void Store::markFound(Token* root)
{
    clearFound();

    root->found = true;

    for (auto itr = ops.rbegin(); itr != ops.rend(); ++itr)
    {
        for (auto b : *itr)
        {
            for (auto c : b)
            {
                Token* const t = c.second;
                if (t->found)
                {
                    if (t->a)
                    {
                        t->a->found = true;
                    }
                    if (t->b)
                    {
                        t->b->found = true;
                    }
                }
            }
        }
    }
}

/*
    Atom* a  = new Atom(i == 0 ? 1.0 : 0.0);
    Atom* b  = new Atom(i == 1 ? 1.0 : 0.0);
    Atom* c  = new Atom(i == 2 ? 1.0 : 0.0);
    Atom* d  = new Atom(0.0);

    Atom* ax = new Atom(OP_MUL, X, a);
    Atom* by = new Atom(OP_MUL, Y, b);
    Atom* cz = new Atom(OP_MUL, Z, c);

    Atom* ax_by = new Atom(OP_ADD, ax, by);
    Atom* cz_d  = new Atom(OP_ADD, cz, d);

    Atom* ax_by_cz_d = new Atom(OP_ADD, ax_by, cz_d);
*/
