/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <fstream>
#include <iostream>
#include <map>
#include <unordered_set>

#include "libfive/tree/tree.hpp"
#include "libfive/tree/archive.hpp"
#include "libfive/eval/eval_array.hpp"
#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/oracle/oracle.hpp"

namespace libfive {

Tree::Tree(float f)
    : Tree(std::make_shared<Data>(TreeConstant { f }))
{
    // Nothing to do here
}

Tree::Tree(std::shared_ptr<const Data> d)
    : std::shared_ptr<const Data>(d)
{
    // Nothing to do here
}

Tree::Tree(std::unique_ptr<const OracleClause>&& o)
    : Tree(std::make_shared<Data>( TreeOracle { std::move(o) }))
{
    // Nothing to do here
}

Tree Tree::var() {
    return Tree::nonary(Opcode::VAR_FREE);
}

Tree Tree::unary(Opcode::Opcode op, const Tree& lhs) {
    // We can only build unary operations with this function
    if (Opcode::args(op) != 1) {
        return invalid();
    }
    // Collapse constant operations
    else if (lhs->op() == Opcode::CONSTANT) {
        auto tmp = Tree(std::make_shared<Data>(TreeUnaryOp { op, lhs }));
        ArrayEvaluator eval(tmp);
        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // abs is idempotent after abs() or square()
    else if (op == Opcode::OP_ABS) {
        if (lhs->op() == Opcode::OP_ABS) {
            return lhs;
        } else if (lhs->op() == Opcode::OP_SQUARE) {
            return lhs;
        }
    }
    // Double-negative returns the original
    else if (op == Opcode::OP_NEG && lhs->op() == Opcode::OP_NEG) {
        return lhs->lhs();
    }
    // Default if we didn't fall into any special cases
    return Tree(std::make_shared<Data>(
           TreeUnaryOp { op, lhs }));
}

Tree Tree::nonary(Opcode::Opcode op) {
    // We can only build unary operations with this function
    if (Opcode::args(op) != 0) {
        return invalid();
    }
    // Default if we didn't fall into any special cases
    return Tree(std::make_shared<Data>(TreeNonaryOp { op }));
}

Tree Tree::binary(Opcode::Opcode op, const Tree& lhs, const Tree& rhs) {
    // We can only build binary operations with this function
    if (Opcode::args(op) != 2) {
        return invalid();
    }
    // Collapse constant operations
    else if (lhs->op() == Opcode::CONSTANT && rhs->op() == Opcode::CONSTANT) {
        auto tmp = Tree(std::make_shared<Data>(TreeBinaryOp { op, lhs, rhs }));
        ArrayEvaluator eval(tmp);
        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // Division by 1 is ignored
    else if (op == Opcode::OP_DIV) {
        if (rhs->op() == Opcode::CONSTANT && rhs->value() == 1.0f) {
            return lhs;
        }
    }
    else if (op == Opcode::OP_ADD) {
        if (lhs->op() == Opcode::CONSTANT && lhs->value() == 0.0f) {
            return rhs;
        } else if (rhs->op() == Opcode::CONSTANT && rhs->value() == 0.0f) {
            return lhs;
        } else if (rhs->op() == Opcode::OP_NEG) {
            const Tree& t = rhs->lhs();
            const Tree& q = lhs;
            return q - t;
        } else if (lhs->op() == Opcode::OP_NEG) {
            return rhs - lhs->lhs();
        }
    } else if (op == Opcode::OP_SUB) {
        if (lhs->op() == Opcode::CONSTANT && lhs->value() == 0.0f) {
            return -rhs;
        } else if (rhs->op() == Opcode::CONSTANT && rhs->value() == 0.0f) {
            return lhs;
        } else if (rhs->op() == Opcode::OP_NEG) {
            return lhs + rhs->lhs();
        }
    } else if (op == Opcode::OP_MUL) {
        if (lhs->op() == Opcode::CONSTANT) {
            if (lhs->value() == 0) {
                return lhs;
            } else if (lhs->value() == 1) {
                return rhs;
            } else if (lhs->value() == -1) {
                return -rhs;
            }
        } else if (rhs->op() == Opcode::CONSTANT) {
            if (rhs->value() == 0) {
                return rhs;
            } else if (rhs->value() == 1) {
                return lhs;
            } else if (rhs->value() == -1) {
                return -lhs;
            }
        } else if (lhs.id() == rhs.id()) {
            return square(lhs);
        }
    } else if (op == Opcode::OP_NTH_ROOT || op == Opcode::OP_POW) {
        if (rhs->op() == Opcode::CONSTANT && rhs->value() == 1.0f) {
            return lhs;
        }
    } else if (op == Opcode::OP_MIN || op == Opcode::OP_MAX) {
        if (lhs.id() == rhs.id()) {
            return lhs;
        }
    }
    // Default if we didn't fall into any special cases
    return Tree(std::make_shared<Data>(
           TreeBinaryOp {op, lhs, rhs}));
}

// Use Meyer's singletons for X/Y/Z, since they're the most common trees
Tree Tree::X() {
    static auto x = Tree::nonary(Opcode::VAR_X);
    return Tree(x);
}
Tree Tree::Y() {
    static auto y = Tree::nonary(Opcode::VAR_Y);
    return Tree(y);
}
Tree Tree::Z() {
    static auto z = Tree::nonary(Opcode::VAR_Z);
    return Tree(z);
}

Tree Tree::invalid() {
    static auto i = std::make_shared<Data>(TreeInvalid {});
    return Tree(i);
}

Tree Tree::with_const_vars() const {
    return Tree::unary(Opcode::CONST_VAR, *this);
}

std::ostream& Tree::print_prefix(std::ostream& s) const {
    std::vector<std::variant<const Data*, char>> todo;
    std::vector<Opcode::Opcode> ops;
    todo.push_back(get());

    while (todo.size()) {
        auto t = todo.back();
        todo.pop_back();
        if (auto c = std::get_if<char>(&t)) {
            switch (*c) {
                case ')': s << ')'; ops.pop_back(); break;
                case '|': ops.pop_back(); break;
                case ' ': s << ' '; break;
            }
        } else if (auto d = std::get_if<const Data*>(&t)) {
            auto op = (**d).op();
            if (op == Opcode::CONSTANT) {
                s << (**d).value();
            } else if (op == Opcode::ORACLE) {
                s << '\'' << (**d).oracle_clause().name();
            } else if (Opcode::args(op) == 0) {
                s << Opcode::toOpString(op);
            } else {
                if (Opcode::isCommutative(op) && ops.size() && ops.back() == op) {
                    todo.push_back('|');
                } else {
                    s << "(" << Opcode::toOpString(op) << " ";
                    todo.push_back(')');
                }
                ops.push_back(op);
                if (Opcode::args(op) == 1) {
                    todo.push_back((**d).lhs().get());
                } else {
                    todo.push_back((**d).rhs().get());
                    todo.push_back(' ');
                    todo.push_back((**d).lhs().get());
                }
            }
        }
    }
    return s;
}

void Tree::serialize(std::ostream& out) const {
    return Archive(*this).serialize(out);
}

Tree Tree::deserialize(std::istream& in) {
    auto s = Archive::deserialize(in).shapes;
    assert(s.size() == 1);
    return s.front().tree;
}

Tree Tree::load(const std::string& filename) {
    std::ifstream in(filename, std::ios::in|std::ios::binary);
    return in.is_open() ? deserialize(in) : invalid();
}

////////////////////////////////////////////////////////////////////////////////

Opcode::Opcode TreeData::op() const {
    if (auto i = std::get_if<TreeNonaryOp>(this)) {
        return i->op;
    } else if (auto i = std::get_if<TreeUnaryOp>(this)) {
        return i->op;
    } else if (auto i = std::get_if<TreeBinaryOp>(this)) {
        return i->op;
    } else if (std::get_if<TreeConstant>(this)) {
        return Opcode::CONSTANT;
    } else if (std::get_if<TreeOracle>(this)) {
        return Opcode::ORACLE;
    } else if (std::get_if<TreeInvalid>(this)) {
        return Opcode::INVALID;
    } else {
        return Opcode::INVALID;
    }
}

const Tree& TreeData::lhs() const {
    if (auto i = std::get_if<TreeUnaryOp>(this)) {
        return i->lhs;
    } else if (auto i = std::get_if<TreeBinaryOp>(this)) {
        return i->lhs;
    } else {
        throw ValueException();
    }
}

const Tree& TreeData::rhs() const {
    if (auto i = std::get_if<TreeBinaryOp>(this)) {
        return i->rhs;
    } else {
        throw ValueException();
    }
}

float TreeData::value() const {
    // Can't use std::get<TreeConstant> because it requires a newer macOS
    // than my main development machine.
    if (auto i = std::get_if<TreeConstant>(this)) {
        return i->value;
    } else {
        throw ValueException();
    }
}

std::unique_ptr<Oracle> TreeData::build_oracle() const {
    if (auto i = std::get_if<TreeOracle>(this)) {
        return i->oracle->getOracle();
    } else {
        throw OracleException();
    }
}

const OracleClause& TreeData::oracle_clause() const {
    if (auto i = std::get_if<TreeOracle>(this)) {
        return *(i->oracle);
    } else {
        throw OracleException();
    }
}

bool Tree::is_valid() const {
    return !std::get_if<TreeInvalid>(get());
}

TreeData::Key TreeData::key() const {
    if (auto d = std::get_if<TreeConstant>(this)) {
        if (std::isnan(d->value)) {
            return Key(true);
        } else {
            return Key(d->value);
        }
    } else if (auto d = std::get_if<TreeNonaryOp>(this)) {
        if (d->op == Opcode::VAR_FREE) {
            return Key(std::make_tuple(d->op, this));
        } else {
            return Key(d->op);
        }
    } else if (auto d = std::get_if<TreeUnaryOp>(this)) {
        return Key(std::make_tuple(d->op, d->lhs.get()));
    } else if (auto d = std::get_if<TreeBinaryOp>(this)) {
        return Key(std::make_tuple(d->op, d->lhs.get(), d->rhs.get()));
    } else if (auto d = std::get_if<TreeOracle>(this)) {
        return Key(d->oracle.get());
    } else {
        return Key(false);
    }
}

Tree Tree::remap(Tree X, Tree Y, Tree Z) const {
    auto flat = walk();

    // If a specific tree (by id) should be remapped, that fact is stored here
    std::unordered_map<Id, Tree> remap = {
            {Tree::X().id(), X},
            {Tree::Y().id(), Y},
            {Tree::Z().id(), Z}};

    for (auto t : flat) {
        Tree changed = Tree::invalid();

        if (auto d = std::get_if<TreeUnaryOp>(t)) {
            auto itr = remap.find(d->lhs.id());
            if (itr != remap.end()) {
                changed = Tree::unary(d->op, itr->second);
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
            auto lhs = remap.find(d->lhs.id());
            auto rhs = remap.find(d->rhs.id());
            if (lhs != remap.end() || rhs != remap.end()) {
                changed = Tree::binary(
                    d->op,
                    (lhs == remap.end()) ? d->lhs
                                         : lhs->second,
                    (rhs == remap.end()) ? d->rhs
                                         : rhs->second );
            }
        } else if (auto d = std::get_if<TreeOracle>(t)) {
            auto r = d->oracle->remap(Tree(t->shared_from_this()), X, Y, Z);
            if (r != d->oracle) {
                changed = Tree(std::move(r));
            }
        }

        if (changed.is_valid()) {
            remap.insert({t, changed});
        }
    }

    auto itr = remap.find(get());
    return (itr == remap.end()) ? *this : itr->second;
}

std::vector<const Tree::Data*> Tree::walk() const {
    // Store how many times each tree (by id) is referenced
    std::unordered_map<Id, unsigned> count;
    std::vector todo = {get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
            if (count[d->rhs.id()]++ == 0) {
                todo.push_back(d->rhs.get());
            }
        }
    }

    // Flatten the tree.  This is a heap-allocated recursive
    // descent, to avoid running into stack limitations.
    todo = {get()};

    std::vector<const Data*> flat;
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        flat.push_back(next);

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            // Schedule child branches to be flattened *after all* of their
            // parents, since we'll be reversing the order of this tape
            // afterwards, meaning children will be evaluated *before all*
            // of their parents.
            if (--count.at(d->lhs.id()) == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (--count.at(d->lhs.id()) == 0) {
                todo.push_back(d->lhs.get());
            }
            if (--count.at(d->rhs.id()) == 0) {
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

Tree Tree::unique() const {
    auto flat = walk();

    // If a specific tree should be remapped, that fact is stored here
    // These remap pointers can point either into the existing tree or
    // to shared_ptrs in the new_ptrs list below, so we store the bare
    // pointer and use shared_from_this to rehydrate it.
    std::unordered_map<Id, const Data*> remap;

    // The canonical tree for each Key is stored here
    std::map<Data::Key, const Data*> canonical;

    // New pointers are owned here, because the maps above hold
    // raw pointers instead of shared_ptrs.
    std::vector<std::shared_ptr<const Data>> new_ptrs;

    for (auto t : flat) {
        // Get canonical key by applying remap to all children
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
        // so remap this tree to the canonical version and keep going.
        if (k_itr != canonical.end()) {
            remap.insert({t, k_itr->second});
        } else if (!changed) {
            // This is the canonical tree, and it requires
            // no remapping, so we're done!
            canonical.insert(k_itr, {key, t});
        } else {
            // We need make a new canonical tree, using remapped arguments
            std::shared_ptr<const Data> out;
            if (auto d = std::get_if<TreeUnaryOp>(t)) {
                auto itr = remap.find(d->lhs.id());
                assert(itr != remap.end());
                out = Tree::unary(d->op, Tree(itr->second->shared_from_this()));
            } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
                auto lhs = remap.find(d->lhs.id());
                auto rhs = remap.find(d->rhs.id());
                assert(lhs != remap.end() || rhs != remap.end());
                out = Tree::binary(
                    d->op,
                    (lhs == remap.end())
                        ? d->lhs
                        : Tree(lhs->second->shared_from_this()),
                    (rhs == remap.end())
                        ? d->rhs
                        : Tree(rhs->second->shared_from_this()));
            }

            // The new tree is the canonical tree; folks that were using
            // the original tree need to use it instead.
            canonical.insert(k_itr, {key, out.get()});
            remap.insert({t, out.get()});

            // The new pointer is owned by the new_ptrs list
            new_ptrs.emplace_back(std::move(out));
        }
    }

    auto itr = remap.find(get());
    return (itr == remap.end())
        ? *this
        : Tree(itr->second->shared_from_this());
}

void Tree::explore_affine(AffineMap& map,
                          std::unordered_map<Tree::Id, float>* prev,
                          float scale) const
{
    const auto op = (*this)->op();

    using namespace Opcode;
    const bool could_be_affine = (op == OP_NEG) ||
        (op == OP_ADD) || (op == OP_SUB) ||
        (op == OP_MUL && ((*this)->lhs()->op() == CONSTANT ||
                          (*this)->rhs()->op() == CONSTANT));

    if (could_be_affine) {
        std::unordered_map<Id, float> my_prev;
        std::unordered_map<Id, float>* prev_ = prev ? prev : &my_prev;

        // Recurse if we haven't already solved for this node
        auto itr = map.find(id());
        if (itr == map.end()) {
            if (op == OP_NEG) {
                (*this)->lhs().explore_affine(map, prev_, -scale);
            } else if (op == OP_ADD) {
                (*this)->lhs().explore_affine(map, prev_, scale);
                (*this)->rhs().explore_affine(map, prev_, scale);
            } else if (op == OP_SUB) {
                (*this)->lhs().explore_affine(map, prev_, scale);
                (*this)->rhs().explore_affine(map, prev_, -scale);
            } else if (op == OP_MUL) {
                if ((*this)->lhs()->op() == CONSTANT) {
                    const float c = (*this)->lhs()->value();
                    (*this)->rhs().explore_affine(map, prev_, scale * c);
                } else if ((*this)->rhs()->op() == CONSTANT) {
                    const float c = (*this)->rhs()->value();
                    (*this)->lhs().explore_affine(map, prev_, scale * c);
                }
            }
        } else if (prev) {
            for (const auto& k: itr->second) {
                (*prev)[k.first] += scale * k.second;
            }
        }

        if (!prev && itr == map.end()) {
            // Record that we should do a remapping
            auto& v = map[id()];
            for (const auto& k: my_prev) {
                v.push_back(k);
            }
        }
    } else {
        // Recurse with parent_was_affine = false
        switch (args(op)) {
            case 2: (*this)->rhs().explore_affine(map, nullptr, 1); // FALLTHROUGH
            case 1: (*this)->lhs().explore_affine(map, nullptr, 1); // FALLTHROUGH
            default: break;
        }

        if (prev) {
            (*prev)[id()] += scale;
        }
    }
}

Tree Tree::collect_affine() const {
    AffineMap map;
    explore_affine(map, nullptr, 1);
    return *this;
}

size_t Tree::size() const {
    std::unordered_set<Id> seen;
    size_t count = 0;

    std::vector<const Data*> todo = {get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        if (!seen.insert(next).second) {
            continue;
        }
        count++;

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            todo.push_back(d->lhs.get());
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            todo.push_back(d->lhs.get());
            todo.push_back(d->rhs.get());
        }
    }
    return count;
}

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode)                      \
Tree name(const Tree& lhs) {                        \
    return Tree::unary(Opcode::opcode, lhs);        \
}
#define OP_BINARY(name, opcode)                     \
Tree name(const Tree& lhs, const Tree& rhs) {       \
    return Tree::binary(Opcode::opcode, lhs, rhs);  \
}
TREE_OPERATORS

////////////////////////////////////////////////////////////////////////////////

std::ostream& operator<<(std::ostream& stream, const Tree& tree)
{
    return tree.print_prefix(stream);
}

}   // namespace libfive
