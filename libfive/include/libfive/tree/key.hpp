/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <boost/functional/hash.hpp>

#include "libfive/tree/tree.hpp"

namespace libfive {

/*  Returns a key suitable for use in maps */
using TreeUnaryKey = std::tuple<
    Opcode::Opcode,
    const TreeData*>;
using TreeBinaryKey = std::tuple<
    Opcode::Opcode,
    const TreeData*,
    const TreeData*>;
using TreeOracleKey = const OracleClause*;
using TreeRemapKey = std::tuple<
    const TreeData*,
    const TreeData*,
    const TreeData*,
    const TreeData*>;
using TreeApplyKey = std::tuple<
    const TreeData*,
    const TreeData*,
    const TreeData*>;

using TreeDataKeyVariant = std::variant<
    bool,               // Used for NaN (true) and invalid (false)
    float,              // Float constants other than NaN
    Opcode::Opcode,     // Nonary operations, other than VAR_FREE
    TreeUnaryKey,       // Unary operations and VAR_FREE
    TreeBinaryKey,      // Binary operations
    TreeRemapKey,       // Remap operation
    TreeApplyKey,       // Apply operation
    TreeOracleKey>;     // Oracles, keyed by their unique_ptr

// Wrapper struct so that we can use forward declarations
struct TreeDataKey : public TreeDataKeyVariant
{
    TreeDataKey()
        : TreeDataKeyVariant(false)
    { /* Nothing to do here */ }

    TreeDataKey(TreeDataKeyVariant&& v)
        : TreeDataKeyVariant(std::move(v))
    { /* Nothing to do here */ }
};

}   // namespace libfive

// Use Boost to implement a hash function for every tuple above
namespace std {
template<typename... T>
struct hash<tuple<T...>> {
    size_t operator()(tuple<T...> const& arg) const noexcept {
        return boost::hash_value(arg);
    }
};

template <>
struct hash<libfive::TreeDataKey> {
    size_t operator()(const libfive::TreeDataKey& k) const {
        const libfive::TreeDataKeyVariant& v = k;
        return hash<libfive::TreeDataKeyVariant>()(v);
    }
};

}   // namespace std
