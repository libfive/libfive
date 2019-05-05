/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>
#include <list>
#include <cmath>
#include <cassert>
#include <array>

#include "libfive/tree/cache.hpp"
#include "libfive/tree/archive.hpp"
#include "libfive/oracle/transformed_oracle_clause.hpp"

namespace Kernel {

Tree::Tree()
    : ptr(nullptr)
{
    // Nothing to do here
}

Tree::Tree(std::unique_ptr<const OracleClause>&& o)
    : ptr(std::shared_ptr<Tree_>(new Tree_{
        Opcode::ORACLE,
        0, // flags
        0, // rank
        std::nanf(""), // value
        std::move(o), // oracle
        nullptr,
        nullptr }))
{
    // Nothing to do here either.  ptr is constructed directly (without using
    // the cache), since using  a unique_ptr to the oracle already precludes
    // duplication.
}

Tree::Tree(float v)
    : ptr(Cache::instance()->constant(v))
{
    // Nothing to do here
}

Tree::Tree(Opcode::Opcode op, Tree a, Tree b, bool simplify)
    : ptr(Cache::instance()->operation(op, a.ptr, b.ptr, simplify))
{
    // Aggressive sanity-checking
    assert((Opcode::args(op) == 0 && a.ptr.get() == 0 && b.ptr.get() == 0) ||
           (Opcode::args(op) == 1 && a.ptr.get() != 0 && b.ptr.get() == 0) ||
           (Opcode::args(op) == 2 && a.ptr.get() != 0 && b.ptr.get() != 0));

    // POW only accepts integral values as its second argument
    if (op == Opcode::OP_POW)
    {
        assert(b->op == Opcode::CONSTANT &&
               b->value == std::round(b->value));
    }
    else if (op == Opcode::OP_NTH_ROOT)
    {
        assert(b->op == Opcode::CONSTANT &&
               b->value == std::round(b->value) &&
               b->value > 0);
    }
}

Tree Tree::var()
{
    return Tree(Cache::instance()->var());
}

Tree::~Tree()
{
    auto lock = Cache::instance();
    ptr.reset();
}

Tree::Tree_::~Tree_()
{
    if (op == Opcode::CONSTANT)
    {
        Cache::instance()->del(value);
    }
    else if (op != Opcode::VAR_FREE && op != Opcode::ORACLE)
    {
        Cache::instance()->del(op, lhs, rhs);
    }
}

std::list<Tree> Tree::ordered() const
{
    std::set<Id> found = {nullptr};
    std::list<std::shared_ptr<Tree_>> todo = { ptr };
    std::map<unsigned, std::list<std::shared_ptr<Tree_>>> ranks;

    while (todo.size())
    {
        auto t = todo.front();
        todo.pop_front();

        if (found.find(t.get()) == found.end())
        {
            todo.push_back(t->lhs);
            todo.push_back(t->rhs);
            found.insert(t.get());
            ranks[t->rank].push_back(t);
        }
    }

    std::list<Tree> out;
    for (auto& r : ranks)
    {
        for (auto& t : r.second)
        {
            out.push_back(Tree(t));
        }
    }
    return out;
}

void Tree::serialize(std::ostream& out) const
{
    return Archive(*this).serialize(out);
}

Tree Tree::deserialize(std::istream& in)
{
    auto s = Archive::deserialize(in).shapes;
    assert(s.size() == 1);
    return s.front().tree;
}

Tree Tree::load(const std::string& filename)
{
    std::ifstream in(filename, std::ios::in|std::ios::binary);
    return in.is_open() ? deserialize(in) : Tree();
}

Tree Tree::remap(Tree X_, Tree Y_, Tree Z_) const
{
    std::map<Tree::Id, Tree> m = {
        {X().id(), X_}, {Y().id(), Y_}, {Z().id(), Z_}};

    return remap(m);
}

Tree Tree::remap(std::map<Id, Tree> m) const
{
    // Oracles only care about remapping X/Y/Z, so we extract them here.
    auto get_remapped = [&](Tree target) {
        auto itr = m.find(target.id());
        return (itr == m.end()) ? target : Tree(itr->second);
    };
    auto X_ = get_remapped(Tree::X());
    auto Y_ = get_remapped(Tree::Y());
    auto Z_ = get_remapped(Tree::Z());

    for (const auto& t : ordered())
    {
        if (Opcode::args(t->op) >= 1)
        {
            auto lhs = m.find(t->lhs.get());
            auto rhs = m.find(t->rhs.get());
            m.insert({t.id(), Tree(Cache::instance()->operation(t->op,
                        lhs == m.end() ? t->lhs : lhs->second.ptr,
                        rhs == m.end() ? t->rhs : rhs->second.ptr))});
        }
        else if (t->op == Opcode::ORACLE)
        {
            m.insert({ t.id(), t->oracle->remap(t, X_, Y_, Z_) });
        }
    }

    // If this Tree was remapped, then return it; otherwise return itself
    auto r = m.find(id());
    return r == m.end() ? *this : Tree(r->second);
}

Tree Tree::lhs() const
{
    return Tree(ptr->lhs);
}

Tree Tree::rhs() const
{
    return Tree(ptr->rhs);
}

Tree Tree::makeVarsConstant() const
{
    std::map<Id, Tree> vars;
    for (auto& o : ordered())
    {
        if (o->op == Opcode::VAR_FREE)
        {
            vars.insert({o.id(),
                    Tree(Cache::instance()->operation(Opcode::CONST_VAR, o.ptr))});
        }
    }
    return remap(vars);
}

////////////////////////////////////////////////////////////////////////////////

const std::shared_ptr<Tree::Tree_> Tree::Tree_::branch(Direction d)
{
    switch (d) {
        case LEFT:  return lhs;
        case RIGHT: return rhs;
    }
    return nullptr;
}

void Tree::Tree_::print(std::ostream& stream, Opcode::Opcode prev_op)
{
    const bool commutative = (prev_op == op);
    const int args = Opcode::args(op);

    if (!commutative)
    {
        switch (args)
        {
            case 2:
            case 1: stream << "(" <<  Opcode::toOpString(op) << " ";
                    break;
            case 0:
                if (op == Opcode::CONSTANT)
                {
                    if (value == int(value))
                    {
                        stream << int(value);
                    }
                    else
                    {
                        stream << value;
                    }
                }
                else if (op == Opcode::ORACLE)
                {
                    stream << "'" << oracle->name();
                }
                else
                {
                    stream << Opcode::toOpString(op);
                }
                break;
            default:    assert(false);
        }
    }

    const auto op_ = Opcode::isCommutative(op) ? op : Opcode::INVALID;
    switch (args)
    {
        case 2:     lhs->print(stream, op_);
                    stream << " ";
                    rhs->print(stream, op_);
                    break;
        case 1:     lhs->print(stream, op_);
                    break;
        case 0:     break;
        default:    assert(false);
    }

    if (!commutative && args > 0)
    {
        stream <<")";
    }
}

}   // namespace Kernel

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode) \
Kernel::Tree name(const Kernel::Tree& a) { return Kernel::Tree(opcode, a); }
OP_UNARY(square,    Kernel::Opcode::OP_SQUARE)
OP_UNARY(sqrt,      Kernel::Opcode::OP_SQRT)
Kernel::Tree Kernel::Tree::operator-() const
    { return Kernel::Tree(Kernel::Opcode::OP_NEG, *this); }
OP_UNARY(abs,       Kernel::Opcode::OP_ABS)
OP_UNARY(sin,       Kernel::Opcode::OP_SIN)
OP_UNARY(cos,       Kernel::Opcode::OP_COS)
OP_UNARY(tan,       Kernel::Opcode::OP_TAN)
OP_UNARY(asin,      Kernel::Opcode::OP_ASIN)
OP_UNARY(acos,      Kernel::Opcode::OP_ACOS)
OP_UNARY(atan,      Kernel::Opcode::OP_ATAN)
OP_UNARY(log,       Kernel::Opcode::OP_LOG)
OP_UNARY(exp,       Kernel::Opcode::OP_EXP)
#undef OP_UNARY

#define OP_BINARY(name, opcode) \
Kernel::Tree name(const Kernel::Tree& a, const Kernel::Tree& b) \
    { return Kernel::Tree(opcode, a, b); }
OP_BINARY(operator+,    Kernel::Opcode::OP_ADD)
OP_BINARY(operator*,    Kernel::Opcode::OP_MUL)
OP_BINARY(min,          Kernel::Opcode::OP_MIN)
OP_BINARY(max,          Kernel::Opcode::OP_MAX)
OP_BINARY(operator-,    Kernel::Opcode::OP_SUB)
OP_BINARY(operator/,    Kernel::Opcode::OP_DIV)
OP_BINARY(atan2,        Kernel::Opcode::OP_ATAN2)
OP_BINARY(pow,          Kernel::Opcode::OP_POW)
OP_BINARY(nth_root,     Kernel::Opcode::OP_NTH_ROOT)
OP_BINARY(mod,          Kernel::Opcode::OP_MOD)
OP_BINARY(nanfill,      Kernel::Opcode::OP_NANFILL)
OP_BINARY(compare,      Kernel::Opcode::OP_COMPARE)
#undef OP_BINARY


std::ostream& operator<<(std::ostream& stream, const Kernel::Tree& tree)
{
    tree->print(stream);
    return stream;
}
