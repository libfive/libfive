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

namespace libfive {

SimpleTree::SimpleTree(float f)
    : SimpleTree(SimpleConstant { f })
{
    // Nothing to do here
}

SimpleTree::SimpleTree(Data d)
    : data(d)
{
    // Nothing to do here
}

SimpleTree SimpleTree::X() {
    return SimpleTree{ SimpleNonaryOp { Opcode::VAR_X }};
}

SimpleTree SimpleTree::Y() {
    return SimpleTree{ SimpleNonaryOp { Opcode::VAR_Y }};
}

SimpleTree SimpleTree::Z() {
    return SimpleTree{ SimpleNonaryOp { Opcode::VAR_Z }};
}

SimpleTree SimpleTree::invalid() {
    return SimpleTree{ SimpleTreeInvalid {} };
}

Opcode::Opcode SimpleTree::op() const {
    if (auto i = std::get_if<SimpleNonaryOp>(&data)) {
        return i->op;
    } else if (auto i = std::get_if<SimpleUnaryOp>(&data)) {
        return i->op;
    } else if (auto i = std::get_if<SimpleBinaryOp>(&data)) {
        return i->op;
    } else if (std::get_if<SimpleConstant>(&data)) {
        return Opcode::CONSTANT;
    } else if (std::get_if<SimpleOracle>(&data)) {
        return Opcode::ORACLE;
    } else if (std::get_if<SimpleTreeInvalid>(&data)) {
        return Opcode::INVALID;
    } else {
        return Opcode::INVALID;
    }
}

SimpleTree SimpleTree::lhs() const {
    if (auto i = std::get_if<SimpleUnaryOp>(&data)) {
        return *i->lhs;
    } else if (auto i = std::get_if<SimpleBinaryOp>(&data)) {
        return *i->lhs;
    } else {
        return invalid();
    }
}

SimpleTree SimpleTree::rhs() const {
    if (auto i = std::get_if<SimpleBinaryOp>(&data)) {
        return *i->rhs;
    } else {
        return invalid();
    }
}

float SimpleTree::value() const {
    // Can't use std::get<SimpleConstant> because it requires a newer macOS
    // than my main development machine.
    if (auto i = std::get_if<SimpleConstant>(&data)) {
        return i->value;
    } else {
        throw Exception();
    }
}

bool SimpleTree::is_valid() const {
    return !std::get_if<SimpleTreeInvalid>(&data);
}

SimpleTree::Key SimpleTree::key() const {
    if (auto d = std::get_if<SimpleConstant>(&data)) {
        if (std::isnan(d->value)) {
            return Key(true);
        } else {
            return Key(d->value);
        }
    } else if (auto d = std::get_if<SimpleNonaryOp>(&data)) {
        return Key(d->op);
    } else if (auto d = std::get_if<SimpleUnaryOp>(&data)) {
        return Key(std::make_tuple(d->op, d->lhs.get()));
    } else if (auto d = std::get_if<SimpleBinaryOp>(&data)) {
        return Key(std::make_tuple(d->op, d->lhs.get(), d->rhs.get()));
    } else {
        return Key(false);
    }
}

SimpleTree SimpleTree::remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const {
    auto head = std::make_shared<SimpleTree>(*this);
    auto flat = flatten(head);

    auto tx = std::make_shared<SimpleTree>(X);
    auto ty = std::make_shared<SimpleTree>(Y);
    auto tz = std::make_shared<SimpleTree>(Z);

    // If a specific tree should be remapped, that fact is stored here
    std::unordered_map<const SimpleTree*, std::shared_ptr<SimpleTree>> remap;

    for (auto t : flat) {
        std::shared_ptr<SimpleTree> changed;

        if (auto d = std::get_if<SimpleNonaryOp>(&(**t).data)) {
            switch (d->op) {
                case Opcode::VAR_X: changed = tx; break;
                case Opcode::VAR_Y: changed = ty; break;
                case Opcode::VAR_Z: changed = tz; break;
                default: break;
            }
        } else if (auto d = std::get_if<SimpleUnaryOp>(&(**t).data)) {
            auto itr = remap.find(d->lhs.get());
            if (itr != remap.end()) {
                changed = std::make_shared<SimpleTree>(SimpleUnaryOp {
                    d->op, itr->second});
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(&(**t).data)) {
            auto lhs = remap.find(d->lhs.get());
            auto rhs = remap.find(d->rhs.get());
            if (lhs != remap.end() || rhs != remap.end()) {
                changed = std::make_shared<SimpleTree>(SimpleBinaryOp {
                    d->op,
                    (lhs == remap.end()) ? d->lhs : lhs->second,
                    (rhs == remap.end()) ? d->rhs : rhs->second });
            }
        }

        if (changed) {
            remap.insert({t->get(), changed});
        }
    }

    auto itr = remap.find(head.get());
    return (itr == remap.end()) ? *head : *itr->second;
}

std::vector<std::shared_ptr<SimpleTree>*> SimpleTree::flatten(
        std::shared_ptr<SimpleTree>& head)
{
    // This block is responsible for flattening the tree
    std::unordered_map<const SimpleTree*, unsigned> count;
    std::vector<std::shared_ptr<SimpleTree>*> todo = {&head};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<SimpleUnaryOp>(&(**next).data)) {
            if (count[d->lhs.get()]++ == 0) {
                todo.push_back(&d->lhs);
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(&(**next).data)) {
            if (count[d->lhs.get()]++ == 0) {
                todo.push_back(&d->lhs);
            }
            if (count[d->rhs.get()]++ == 0) {
                todo.push_back(&d->rhs);
            }
        }
    }

    // Flatten the tree.  This is a heap-allocated recursive
    // descent, to avoid running into stack limitations.
    todo = {&head};

    std::vector<std::shared_ptr<SimpleTree>*> flat;
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        flat.push_back(next);

        if (auto d = std::get_if<SimpleUnaryOp>(&(**next).data)) {
            // Schedule child branches to be flattened *after all* of their
            // parents, since we'll be reversing the order of this tape
            // afterwards, meaning children will be evaluated *before all*
            // of their parents.
            if (--count.at(d->lhs.get()) == 0) {
                todo.push_back(&d->lhs);
            }
        } else if (auto d = std::get_if<SimpleBinaryOp>(&(**next).data)) {
            if (--count.at(d->lhs.get()) == 0) {
                todo.push_back(&d->lhs);
            }
            if (--count.at(d->rhs.get()) == 0) {
                todo.push_back(&d->rhs);
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
    auto head = std::make_shared<SimpleTree>(*this);
    auto flat = flatten(head);

    // If a specific tree should be remapped, that fact is stored here
    std::unordered_map<const SimpleTree*, std::shared_ptr<SimpleTree>> remap;
    // The canonical tree for each Key is stored here
    std::map<Key, std::shared_ptr<SimpleTree>> canonical;

    for (auto t : flat) {
        // Get canonical key
        auto key = (**t).key();
        bool changed = false;
        if (auto k = std::get_if<UnaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second.get();
                changed = true;
            }
        } else if (auto k = std::get_if<BinaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second.get();
                changed = true;
            }
            itr = remap.find(std::get<2>(*k));
            if (itr != remap.end()) {
                std::get<2>(*k) = itr->second.get();
                changed = true;
            }
        }

        auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remap it and keep going.
        if (k_itr != canonical.end()) {
            remap.insert({t->get(), k_itr->second});
        } else if (!changed) {
            // This is the canonical tree, and it requires
            // no remapping, so we're done!
            canonical.insert(k_itr, {key, *t});
        } else {
            // We need make a new canonical tree, using remapped arguments
            auto out = std::make_shared<SimpleTree>(**t);
            if (auto d = std::get_if<SimpleUnaryOp>(&out->data)) {
                auto itr = remap.find(d->lhs.get());
                if (itr != remap.end()) {
                    d->lhs = itr->second;
                }
            } else if (auto d = std::get_if<SimpleBinaryOp>(&out->data)) {
                auto itr = remap.find(d->lhs.get());
                if (itr != remap.end()) {
                    d->lhs = itr->second;
                }
                itr = remap.find(d->rhs.get());
                if (itr != remap.end()) {
                    d->rhs = itr->second;
                }
            }
            // The new tree is the canonical tree; folks that were using
            // the original tree need to use it instead.
            canonical.insert(k_itr, {key, out});
            remap.insert({t->get(), out});
        }
    }

    auto itr = remap.find(head.get());
    return (itr == remap.end()) ? *head : *itr->second;
}

size_t SimpleTree::size() const {
    std::unordered_set<const SimpleTree*> seen;
    size_t count = 0;

    std::vector<const SimpleTree*> todo = {this};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        if (!seen.insert(next).second) {
            continue;
        }
        count++;

        if (auto d = std::get_if<SimpleUnaryOp>(&(*next).data)) {
            todo.push_back(d->lhs.get());
        } else if (auto d = std::get_if<SimpleBinaryOp>(&(*next).data)) {
            todo.push_back(d->lhs.get());
            todo.push_back(d->rhs.get());
        }
    }
    return count;
}

}   // namespace libfive

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode) \
libfive::SimpleTree name(const libfive::SimpleTree& a) {        \
    return libfive::SimpleTree { libfive::SimpleUnaryOp {       \
        opcode,                                                 \
        std::make_shared<libfive::SimpleTree>(a) }};            \
}
OP_UNARY(square,    libfive::Opcode::OP_SQUARE)
OP_UNARY(sqrt,      libfive::Opcode::OP_SQRT)
OP_UNARY(abs,       libfive::Opcode::OP_ABS)
OP_UNARY(sin,       libfive::Opcode::OP_SIN)
OP_UNARY(cos,       libfive::Opcode::OP_COS)
OP_UNARY(tan,       libfive::Opcode::OP_TAN)
OP_UNARY(asin,      libfive::Opcode::OP_ASIN)
OP_UNARY(acos,      libfive::Opcode::OP_ACOS)
OP_UNARY(atan,      libfive::Opcode::OP_ATAN)
OP_UNARY(log,       libfive::Opcode::OP_LOG)
OP_UNARY(exp,       libfive::Opcode::OP_EXP)
#undef OP_UNARY

libfive::SimpleTree libfive::SimpleTree::operator-() const {
    return libfive::SimpleTree{ libfive::SimpleUnaryOp {
            libfive::Opcode::OP_NEG,
            std::make_shared<libfive::SimpleTree>(*this) }};
}

#define OP_BINARY(name, opcode)                                         \
libfive::SimpleTree name(const libfive::SimpleTree& a,                  \
                         const libfive::SimpleTree& b) {                \
    if (&a == &b) {                                                     \
        auto t = std::make_shared<libfive::SimpleTree>(a);              \
        return libfive::SimpleTree { libfive::SimpleBinaryOp {          \
                opcode, t, t } };                                       \
    } else {                                                            \
        return libfive::SimpleTree { libfive::SimpleBinaryOp {          \
                opcode, std::make_shared<libfive::SimpleTree>(a),       \
                        std::make_shared<libfive::SimpleTree>(b) }};    \
    }                                                                   \
}
OP_BINARY(operator+,    libfive::Opcode::OP_ADD)
OP_BINARY(operator*,    libfive::Opcode::OP_MUL)
OP_BINARY(min,          libfive::Opcode::OP_MIN)
OP_BINARY(max,          libfive::Opcode::OP_MAX)
OP_BINARY(operator-,    libfive::Opcode::OP_SUB)
OP_BINARY(operator/,    libfive::Opcode::OP_DIV)
OP_BINARY(atan2,        libfive::Opcode::OP_ATAN2)
OP_BINARY(pow,          libfive::Opcode::OP_POW)
OP_BINARY(nth_root,     libfive::Opcode::OP_NTH_ROOT)
OP_BINARY(mod,          libfive::Opcode::OP_MOD)
OP_BINARY(nanfill,      libfive::Opcode::OP_NANFILL)
OP_BINARY(compare,      libfive::Opcode::OP_COMPARE)
#undef OP_BINARY
