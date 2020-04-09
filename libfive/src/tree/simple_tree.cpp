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

SimpleTree SimpleTree::clone() const {
    // remap() does a deep clone, so do a dummy remapping here
    return remap(X(), Y(), Z());
}

SimpleTree SimpleTree::remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const {
    using P = std::pair<const SimpleTree*, std::shared_ptr<SimpleTree>&>;
    auto out = std::make_shared<SimpleTree>(SimpleTree::invalid());

    std::vector<P> todo = {{this, out}};
    std::unordered_map<const SimpleTree*, std::shared_ptr<SimpleTree>> done;

    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<SimpleNonaryOp>(&next.first->data)) {
            switch (d->op) {
                case Opcode::VAR_X: *next.second = X; break;
                case Opcode::VAR_Y: *next.second = Y; break;
                case Opcode::VAR_Z: *next.second = Z; break;
                default: *next.second = *next.first;
            }
        } else if (auto d = std::get_if<SimpleUnaryOp>(&next.first->data)) {
            std::shared_ptr<SimpleTree> lhs;
            // Check to see if we've already visited the child branches;
            // if so, use that shared_ptr instead of making a new one.
            auto itr = done.find(d->lhs.get());
            if (itr != done.end()) {
                lhs = itr->second;
            } else {
                lhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->lhs.get(), lhs});
                done.insert(itr, {d->lhs.get(), lhs});
            }
            *next.second = SimpleTree { SimpleUnaryOp { d->op, lhs }};
        } else if (auto d = std::get_if<SimpleBinaryOp>(&next.first->data)) {
            std::shared_ptr<SimpleTree> lhs, rhs;
            auto itr = done.find(d->lhs.get());
            if (itr != done.end()) {
                lhs = itr->second;
            } else {
                lhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->lhs.get(), lhs});
                done.insert(itr, {d->lhs.get(), lhs});
            }
            itr = done.find(d->rhs.get());
            if (itr != done.end()) {
                rhs = itr->second;
            } else {
                rhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->rhs.get(), rhs});
                done.insert(itr, {d->rhs.get(), rhs});
            }
            *next.second = SimpleTree { SimpleBinaryOp { d->op, lhs, rhs }};
        } else if (auto d = std::get_if<SimpleConstant>(&next.first->data)) {
            *next.second = *next.first;
        } else if (auto d = std::get_if<SimpleOracle>(&next.first->data)) {
            // TODO
            *next.second = *next.first;
        }
    }
    return *out;
}

SimpleTree SimpleTree::unique() const {
    auto head = std::make_shared<SimpleTree>(*this);
    std::vector<std::shared_ptr<SimpleTree>*> flat;

    {   // This block is responsible for flattening the tree
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
    }

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

#define OP_BINARY(name, opcode)                                     \
libfive::SimpleTree name(const libfive::SimpleTree& a,              \
                         const libfive::SimpleTree& b) {            \
    return libfive::SimpleTree { libfive::SimpleBinaryOp {          \
            opcode, std::make_shared<libfive::SimpleTree>(a),       \
                    std::make_shared<libfive::SimpleTree>(b) }};    \
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
