/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <vector>
#include <unordered_map>

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

SimpleTree SimpleTree::clone() const {
    using P = std::pair<const SimpleTree*, SimpleTree*>;
    auto out = SimpleTree::invalid();

    std::vector<P> todo = {{this, &out}};
    std::unordered_map<const SimpleTree*, std::shared_ptr<SimpleTree>> done;

    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<SimpleNonaryOp>(&next.first->data)) {
            *next.second = SimpleTree { SimpleNonaryOp { d->op }};
        } else if (auto d = std::get_if<SimpleUnaryOp>(&next.first->data)) {
            std::shared_ptr<SimpleTree> lhs;
            // Check to see if we've already visited the child branches;
            // if so, use that shared_ptr instead of making a new one.
            auto itr = done.find(d->lhs.get());
            if (itr != done.end()) {
                lhs = itr->second;
            } else {
                lhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->lhs.get(), lhs.get()});
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
                todo.push_back({d->lhs.get(), lhs.get()});
                done.insert(itr, {d->lhs.get(), lhs});
            }
            itr = done.find(d->rhs.get());
            if (itr != done.end()) {
                rhs = itr->second;
            } else {
                rhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->rhs.get(), rhs.get()});
                done.insert(itr, {d->rhs.get(), rhs});
            }
            *next.second = SimpleTree { SimpleBinaryOp { d->op, lhs, rhs }};
        } else if (auto d = std::get_if<SimpleConstant>(&next.first->data)) {
            *next.second = SimpleTree { SimpleConstant { d->value }};
        } else if (auto d = std::get_if<SimpleOracle>(&next.first->data)) {
            *next.second = SimpleTree { SimpleOracle { d->oracle }};
        }
    }
    return out;
}

SimpleTree SimpleTree::remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const {
    // This is basically the same as clone() above, but with extra logic
    using P = std::pair<const SimpleTree*, SimpleTree*>;
    auto out = SimpleTree::invalid();

    std::vector<P> todo = {{this, &out}};
    std::unordered_map<const SimpleTree*, std::shared_ptr<SimpleTree>> done;

    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<SimpleNonaryOp>(&next.first->data)) {
            switch (d->op) {
                case Opcode::VAR_X: *next.second = X; break;
                case Opcode::VAR_Y: *next.second = Y; break;
                case Opcode::VAR_Z: *next.second = Z; break;
                default: *next.second = SimpleTree { SimpleNonaryOp { d->op }};
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
                todo.push_back({d->lhs.get(), lhs.get()});
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
                todo.push_back({d->lhs.get(), lhs.get()});
                done.insert(itr, {d->lhs.get(), lhs});
            }
            itr = done.find(d->rhs.get());
            if (itr != done.end()) {
                rhs = itr->second;
            } else {
                rhs = std::make_shared<SimpleTree>(invalid());
                todo.push_back({d->rhs.get(), rhs.get()});
                done.insert(itr, {d->rhs.get(), rhs});
            }
            *next.second = SimpleTree { SimpleBinaryOp { d->op, lhs, rhs }};
        } else if (auto d = std::get_if<SimpleConstant>(&next.first->data)) {
            *next.second = SimpleTree { SimpleConstant { d->value }};
        } else if (auto d = std::get_if<SimpleOracle>(&next.first->data)) {
            // TODO
            *next.second = SimpleTree { SimpleOracle { d->oracle }};
        }
    }
    return out;
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
