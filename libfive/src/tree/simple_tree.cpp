#include <iostream>
/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <map>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include "libfive/tree/simple_tree.hpp"
#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/oracle/oracle.hpp"

namespace libfive {

SimpleTree::SimpleTree(float f)
    : SimpleTree(std::make_shared<Data>(SimpleConstant { f }))
{
    // Nothing to do here
}

SimpleTree::SimpleTree(std::shared_ptr<const Data> d)
    : data(d)
{
    // Nothing to do here
}

// Use Meyer's singletons for X/Y/Z, since they're the most common trees
SimpleTree SimpleTree::X() {
    static auto x = std::make_shared<Data>(SimpleNonaryOp { Opcode::VAR_X });
    return SimpleTree(x);
}
SimpleTree SimpleTree::Y() {
    static auto y = std::make_shared<Data>(SimpleNonaryOp { Opcode::VAR_Y });
    return SimpleTree(y);
}
SimpleTree SimpleTree::Z() {
    static auto z = std::make_shared<Data>(SimpleNonaryOp { Opcode::VAR_Z });
    return SimpleTree(z);
}

SimpleTree SimpleTree::invalid() {
    static auto i = std::make_shared<Data>(SimpleTreeInvalid {});
    return SimpleTree(i);
}

////////////////////////////////////////////////////////////////////////////////

Opcode::Opcode SimpleTreeData::op() const {
    if (auto i = std::get_if<SimpleNonaryOp>(this)) {
        return i->op;
    } else if (auto i = std::get_if<SimpleUnaryOp>(this)) {
        return i->op;
    } else if (auto i = std::get_if<SimpleBinaryOp>(this)) {
        return i->op;
    } else if (std::get_if<SimpleConstant>(this)) {
        return Opcode::CONSTANT;
    } else if (std::get_if<SimpleOracle>(this)) {
        return Opcode::ORACLE;
    } else if (std::get_if<SimpleTreeInvalid>(this)) {
        return Opcode::INVALID;
    } else {
        return Opcode::INVALID;
    }
}

const SimpleTreeData* SimpleTreeData::lhs() const {
    if (auto i = std::get_if<SimpleUnaryOp>(this)) {
        return i->lhs.get();
    } else if (auto i = std::get_if<SimpleBinaryOp>(this)) {
        return i->lhs.get();
    } else {
        return nullptr;
    }
}

const SimpleTreeData* SimpleTreeData::rhs() const {
    if (auto i = std::get_if<SimpleBinaryOp>(this)) {
        return i->rhs.get();
    } else {
        return nullptr;
    }
}

float SimpleTreeData::value() const {
    // Can't use std::get<SimpleConstant> because it requires a newer macOS
    // than my main development machine.
    if (auto i = std::get_if<SimpleConstant>(this)) {
        return i->value;
    } else {
        throw ValueException();
    }
}

std::unique_ptr<Oracle> SimpleTreeData::build_oracle() const {
    if (auto i = std::get_if<SimpleOracle>(this)) {
        return i->oracle->getOracle();
    } else {
        throw OracleException();
    }
}

const OracleClause* SimpleTreeData::oracle_clause() const {
    if (auto i = std::get_if<SimpleOracle>(this)) {
        return i->oracle.get();
    } else {
        throw OracleException();
    }
}

bool SimpleTree::is_valid() const {
    return !std::get_if<SimpleTreeInvalid>(data.get());
}

SimpleTreeData::Key SimpleTreeData::key() const {
    if (auto d = std::get_if<SimpleConstant>(this)) {
        if (std::isnan(d->value)) {
            return Key(true);
        } else {
            return Key(d->value);
        }
    } else if (auto d = std::get_if<SimpleNonaryOp>(this)) {
        return Key(d->op);
    } else if (auto d = std::get_if<SimpleUnaryOp>(this)) {
        return Key(std::make_tuple(d->op, d->lhs.get()));
    } else if (auto d = std::get_if<SimpleBinaryOp>(this)) {
        return Key(std::make_tuple(d->op, d->lhs.get(), d->rhs.get()));
    } else {
        return Key(false);
    }
}

SimpleTree SimpleTree::remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const {
    auto flat = walk();

    // If a specific tree should be remapped, that fact is stored here
    std::unordered_map<const Data*, std::shared_ptr<const Data>> remap;

    for (auto t : flat) {
        std::shared_ptr<const Data> changed;

        if (auto d = std::get_if<SimpleNonaryOp>(t)) {
            switch (d->op) {
                case Opcode::VAR_X: changed = X.data; break;
                case Opcode::VAR_Y: changed = Y.data; break;
                case Opcode::VAR_Z: changed = Z.data; break;
                default: break;
            }
        } else if (auto d = std::get_if<SimpleUnaryOp>(t)) {
            auto itr = remap.find(d->lhs.get());
            if (itr != remap.end()) {
                changed = std::make_shared<Data>(SimpleUnaryOp {
                    d->op, itr->second->shared_from_this()});
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(t)) {
            auto lhs = remap.find(d->lhs.get());
            auto rhs = remap.find(d->rhs.get());
            if (lhs != remap.end() || rhs != remap.end()) {
                changed = std::make_shared<Data>(SimpleBinaryOp {
                    d->op,
                    (lhs == remap.end()) ? d->lhs
                                         : lhs->second->shared_from_this(),
                    (rhs == remap.end()) ? d->rhs
                                         : rhs->second->shared_from_this() });
            }
        }

        if (changed) {
            remap.insert({t, changed});
        }
    }

    auto itr = remap.find(data.get());
    return (itr == remap.end()) ? *this : SimpleTree(itr->second->shared_from_this());
}

std::vector<const SimpleTree::Data*> SimpleTree::walk() const {
    // This block is responsible for flattening the tree
    std::unordered_map<const Data*, unsigned> count;
    std::vector todo = {data.get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<SimpleUnaryOp>(next)) {
            if (count[d->lhs.get()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(next)) {
            if (count[d->lhs.get()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
            if (count[d->rhs.get()]++ == 0) {
                todo.push_back(d->rhs.get());
            }
        }
    }

    // Flatten the tree.  This is a heap-allocated recursive
    // descent, to avoid running into stack limitations.
    todo = {data.get()};

    std::vector<const Data*> flat;
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        flat.push_back(next);

        if (auto d = std::get_if<SimpleUnaryOp>(next)) {
            // Schedule child branches to be flattened *after all* of their
            // parents, since we'll be reversing the order of this tape
            // afterwards, meaning children will be evaluated *before all*
            // of their parents.
            if (--count.at(d->lhs.get()) == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(next)) {
            if (--count.at(d->lhs.get()) == 0) {
                todo.push_back(d->lhs.get());
            }
            if (--count.at(d->rhs.get()) == 0) {
                todo.push_back(d->rhs.get());
            }
        }
    }
    // We'll walk from the leafs up to the root, storing the first
    // unique instance of a given operation in the maps above, and
    // marking subsequent instances in the remap table.
    std::reverse(flat.begin(), flat.end());

    return flat;
}

SimpleTree SimpleTree::unique() const {
    auto flat = walk();

    // If a specific tree should be remapped, that fact is stored here
    // These remap pointers can point either into the existing tree or
    // to shared_ptrs in the new_ptrs list below
    std::unordered_map<const Data*, const Data*> remap;

    // The canonical tree for each Key is stored here
    std::map<Data::Key, const Data*> canonical;

    // New pointers are owned here, because the maps above hold
    // raw pointers instead of shared_ptrs.
    std::vector<std::shared_ptr<const Data>> new_ptrs;

    for (auto t : flat) {
        // Get canonical key
        auto key = t->key();
        bool changed = false;
        if (auto k = std::get_if<Data::UnaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
        } else if (auto k = std::get_if<Data::BinaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
            itr = remap.find(std::get<2>(*k));
            if (itr != remap.end()) {
                std::get<2>(*k) = itr->second;
                changed = true;
            }
        }

        auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remap it and keep going.
        if (k_itr != canonical.end()) {
            remap.insert({t, k_itr->second});
        } else if (!changed) {
            // This is the canonical tree, and it requires
            // no remapping, so we're done!
            canonical.insert(k_itr, {key, t});
        } else {
            // We need make a new canonical tree, using remapped arguments
            auto out = std::make_shared<Data>(*t);
            if (auto d = std::get_if<SimpleUnaryOp>(out.get())) {
                auto itr = remap.find(d->lhs.get());
                if (itr != remap.end()) {
                    d->lhs = itr->second->shared_from_this();
                }
            } else if (auto d = std::get_if<SimpleBinaryOp>(out.get())) {
                auto itr = remap.find(d->lhs.get());
                if (itr != remap.end()) {
                    d->lhs = itr->second->shared_from_this();
                }
                itr = remap.find(d->rhs.get());
                if (itr != remap.end()) {
                    d->rhs = itr->second->shared_from_this();
                }
            }
            // TODO: handle identities, e.g. min(a, a) => a
            // or a * a => square(a), which have only been proven now
            // that children are deduplicated

            // The new tree is the canonical tree; folks that were using
            // the original tree need to use it instead.
            canonical.insert(k_itr, {key, out.get()});
            remap.insert({t, out.get()});

            // The new pointer is owned by the new_ptrs list
            new_ptrs.emplace_back(std::move(out));
        }
    }

    auto itr = remap.find(data.get());
    return (itr == remap.end())
        ? *this
        : SimpleTree(itr->second->shared_from_this());
}

size_t SimpleTree::size() const {
    std::unordered_set<const Data*> seen;
    size_t count = 0;

    std::vector<const Data*> todo = {data.get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        if (!seen.insert(next).second) {
            continue;
        }
        count++;

        if (auto d = std::get_if<SimpleUnaryOp>(next)) {
            todo.push_back(d->lhs.get());
        } else if (auto d = std::get_if<SimpleBinaryOp>(next)) {
            todo.push_back(d->lhs.get());
            todo.push_back(d->rhs.get());
        }
    }
    return count;
}

SimpleTree SimpleTree::lhs() const {
    const auto ptr = data->lhs();
    return ptr ? SimpleTree(ptr->shared_from_this()) : invalid();
}

SimpleTree SimpleTree::rhs() const {
    const auto ptr = data->rhs();
    return ptr ? SimpleTree(ptr->shared_from_this()) : invalid();
}

}   // namespace libfive

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode) \
libfive::SimpleTree name(const libfive::SimpleTree& a) {                    \
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>( \
        libfive::SimpleUnaryOp {                                            \
            opcode,                                                         \
            a.data }));                                                     \
}
OP_UNARY(square,    libfive::Opcode::OP_SQUARE)
OP_UNARY(sqrt,      libfive::Opcode::OP_SQRT)
OP_UNARY(sin,       libfive::Opcode::OP_SIN)
OP_UNARY(cos,       libfive::Opcode::OP_COS)
OP_UNARY(tan,       libfive::Opcode::OP_TAN)
OP_UNARY(asin,      libfive::Opcode::OP_ASIN)
OP_UNARY(acos,      libfive::Opcode::OP_ACOS)
OP_UNARY(atan,      libfive::Opcode::OP_ATAN)
OP_UNARY(log,       libfive::Opcode::OP_LOG)
OP_UNARY(exp,       libfive::Opcode::OP_EXP)
#undef OP_UNARY

#define OP_BINARY(name, opcode)                                             \
libfive::SimpleTree name(const libfive::SimpleTree& a,                      \
                         const libfive::SimpleTree& b) {                    \
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>( \
        libfive::SimpleBinaryOp {                                           \
            opcode, a.data, b.data }));                                     \
}
OP_BINARY(atan2,        libfive::Opcode::OP_ATAN2)
OP_BINARY(mod,          libfive::Opcode::OP_MOD)
OP_BINARY(nanfill,      libfive::Opcode::OP_NANFILL)
OP_BINARY(compare,      libfive::Opcode::OP_COMPARE)
#undef OP_BINARY

////////////////////////////////////////////////////////////////////////////////
// Special-casing for arithmetic identities goes here
libfive::SimpleTree abs(const libfive::SimpleTree& a) {
    if (a.op() == libfive::Opcode::OP_ABS) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleUnaryOp {
        libfive::Opcode::OP_ABS,
            a.data }));
}

libfive::SimpleTree libfive::SimpleTree::operator-() const {
    if (op() == Opcode::OP_NEG) {
        // Double-negative returns the original value
        return SimpleTree(lhs());
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleUnaryOp {
            libfive::Opcode::OP_NEG, data }));
}

libfive::SimpleTree operator+(const libfive::SimpleTree& a,
                              const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (a.op() == Opcode::CONSTANT && a.value() == 0.0f) {
        return b;
    } else if (b.op() == Opcode::CONSTANT && b.value() == 0.0f) {
        return a;
    } else if (b.op() == Opcode::OP_NEG) {
        return a - b.lhs();
    } else if (a.op() == Opcode::OP_NEG) {
        return b - a.lhs();
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_ADD, a.data, b.data }));
}

libfive::SimpleTree operator-(const libfive::SimpleTree& a,
                              const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (a.op() == Opcode::CONSTANT && a.value() == 0.0f) {
        return -b;
    } else if (b.op() == Opcode::CONSTANT && b.value() == 0.0f) {
        return a;
    } else if (b.op() == Opcode::OP_NEG) {
        return a + b.lhs();
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_SUB, a.data, b.data }));
}

libfive::SimpleTree operator*(const libfive::SimpleTree& a,
                              const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (a.op() == Opcode::CONSTANT) {
        if (a.value() == 0) {
            return a;
        } else if (a.value() == 1) {
            return b;
        } else if (a.value() == -1) {
            return -b;
        }
    } else if (b.op() == Opcode::CONSTANT) {
        if (b.value() == 0) {
            return b;
        } else if (b.value() == 1) {
            return a;
        } else if (b.value() == -1) {
            return -a;
        }
    } else if (a.data.get() == b.data.get()) {
        return square(a);
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_MUL, a.data, b.data }));
}

libfive::SimpleTree nth_root(const libfive::SimpleTree& a,
                             const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (b.op() == Opcode::CONSTANT && b.value() == 1.0f) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_NTH_ROOT, a.data, b.data }));
}

libfive::SimpleTree pow(const libfive::SimpleTree& a,
                        const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (b.op() == Opcode::CONSTANT && b.value() == 1.0f) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_POW, a.data, b.data }));
}

libfive::SimpleTree min(const libfive::SimpleTree& a,
                        const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (a.data.get() == b.data.get()) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_MIN, a.data, b.data }));
}

libfive::SimpleTree max(const libfive::SimpleTree& a,
                        const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (a.data.get() == b.data.get()) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_MAX, a.data, b.data }));
}

libfive::SimpleTree operator/(const libfive::SimpleTree& a,
                              const libfive::SimpleTree& b)
{
    using namespace libfive;
    if (b.op() == Opcode::CONSTANT && b.value() == 1.0f) {
        return a;
    }
    return libfive::SimpleTree(std::make_shared<libfive::SimpleTree::Data>(
        libfive::SimpleBinaryOp {
        libfive::Opcode::OP_DIV, a.data, b.data }));
}
