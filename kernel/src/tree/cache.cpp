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

#include "ao/kernel/tree/cache.hpp"
#include "ao/kernel/tree/tree.hpp"

/******************************************************************************
 * Per-thread cache lookup
 ******************************************************************************/
std::map<std::thread::id, std::shared_ptr<Cache>> Cache::instances;
std::mutex Cache::instance_lock;

std::shared_ptr<Cache> Cache::instance()
{
    auto id = std::this_thread::get_id();

    std::lock_guard<std::mutex> g(instance_lock);
    if (instances.find(id) == instances.end())
    {
        instances[id].reset(new Cache);
    }
    return instances[id];
}

void Cache::reset()
{
    auto id = std::this_thread::get_id();

    std::lock_guard<std::mutex> g(instance_lock);
    auto itr = instances.find(id);
    if (itr != instances.end())
    {
        instances.erase(itr);
    }
}

/******************************************************************************
 * Id constructors
 ******************************************************************************/
Cache::Id Cache::constant(float v)
{
    auto k = key(v);
    if (data.left.find(k) == data.left.end())
    {
        data.insert({k, next++});
    }
    return data.left.at(k);
}

Cache::Id Cache::operation(Opcode::Opcode op, Id a, Id b, bool collapse)
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

    // Otherwise, construct a new Id and add it to the ops set
    auto k = key(op, a, b);
    if (data.left.find(k) == data.left.end())
    {
        data.insert({k, next++});
    }

    return data.left.at(k);
}

Cache::Id Cache::affine(float a, float b, float c, float d)
{
    // Build up the desired tree structure with collapse = false
    // to keep branches from automatically collapsing.
    return operation(Opcode::AFFINE_VEC,
            operation(Opcode::ADD,
                operation(Opcode::MUL, X(), constant(a), false),
                operation(Opcode::MUL, Y(), constant(b), false), false),
            operation(Opcode::ADD,
                operation(Opcode::MUL, Z(), constant(c), false),
                constant(d), false));
}

/******************************************************************************
 * Clause simplification
 ******************************************************************************/
Cache::Id Cache::checkAffine(Opcode::Opcode op, Id a, Id b)
{
    if (Opcode::args(op) != 2)
    {
        return 0;
    }

    // Pull op-codes from both branches
    auto op_a = opcode(a);
    auto op_b = opcode(b);

    if (op == Opcode::ADD)
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
            auto va = getAffine(a);
            auto vb = getAffine(b);
            return affine(va + vb);
        }
    }
    else if (op == Opcode::SUB)
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
            auto va = getAffine(a);
            auto vb = getAffine(b);
            return affine(va - vb);
        }
    }
    else if (op == Opcode::MUL)
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
    else if (op == Opcode::DIV)
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

Cache::Id Cache::checkIdentity(Opcode::Opcode op, Id a, Id b)
{
    if (Opcode::args(op) != 2)
    {
        return 0;
    }

    // Pull op-codes from both branches
    auto op_a = opcode(a);
    auto op_b = opcode(b);

    // Special cases to handle identity operations
    if (op == Opcode::ADD)
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
    else if (op == Opcode::SUB)
    {
        if (op_a == Opcode::CONST && value(a) == 0)
        {
            return operation(Opcode::NEG, b);
        }
        else if (op_b == Opcode::CONST && value(b) == 0)
        {
            return a;
        }
    }
    else if (op == Opcode::MUL)
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
glm::vec4 Cache::getAffine(Id root, bool* success) const
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
Cache::Key Cache::key(float v) const
{
    return Key(v);
}

Cache::Key Cache::key(Opcode::Opcode op, Id a, Id b) const
{
    return Key(op, a, b, std::max(a ? rank(a) + 1 : 0,
                                  b ? rank(b) + 1 : 0));
}

/******************************************************************************
 * Tree walking and modification
 ******************************************************************************/
std::set<Cache::Id> Cache::findConnected(Id root)
{
    std::set<Id> found = {root};

    // Iterate over weight levels from top to bottom
    for (auto c = data.left.rbegin(); c != data.left.rend(); ++c)
    {
        if (found.find(c->second) != found.end())
        {
            found.insert(c->first.lhs());
            found.insert(c->first.rhs());
        }
    }

    return found;
}

Cache::Id Cache::rebuild(Id root, std::map<Id, Id> changed)
{
    // Deep copy of clauses so that changes don't invalidate iterators
    decltype(data) tokens = data;

    // Iterate over weight levels from bottom to top
    for (auto c : tokens.left)
    {
        // Get child Ids
        auto a = c.first.lhs();
        auto b = c.first.rhs();

        // If either of the child pointers has changed, regenerate
        // the operation to ensure correct pointers and weight
        if (changed.count(a) || changed.count(b))
        {
            changed[c.second] = operation(c.first.opcode(),
                changed.count(a) ? changed[a] : a,
                changed.count(b) ? changed[b] : b);
        }
    }

    return changed.count(root) ? changed[root] : root;
}

Cache::Id Cache::collapse(Id root)
{
    // Deep copy of clauses so that changes don't invalidate iterators
    decltype(data) tokens = data;

    // Details on which nodes have changed
    std::map<Id, Id> changed;

    // Turn every AFFINE into a normal OP_ADD
    // (with identity operations automatically cancelled out)
    for (auto c : tokens.left)
    {
        if (c.first.opcode() == Opcode::AFFINE_VEC)
        {
            auto v = getAffine(c.second);
            changed[c.second] = operation(Opcode::ADD,
                    operation(Opcode::ADD,
                        operation(Opcode::MUL, X(), constant(v.x)),
                        operation(Opcode::MUL, Y(), constant(v.y))),
                    operation(Opcode::ADD,
                        operation(Opcode::MUL, Z(), constant(v.z)),
                        constant(v.w)));
        }
    }

    return rebuild(root, changed);
}
