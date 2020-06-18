/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/tree/tree.hpp"

namespace libfive {

struct TreeNonaryOp {
    Opcode::Opcode op;
};
struct TreeUnaryOp {
    Opcode::Opcode op;
    Tree lhs;
};
struct TreeBinaryOp {
    Opcode::Opcode op;
    Tree lhs;
    Tree rhs;
};
struct TreeConstant {
    float value;
};
struct TreeOracle {
    std::unique_ptr<const OracleClause> oracle;
};
struct TreeInvalid {
    // No members
};
using TreeDataVariant = std::variant<
        TreeNonaryOp,
        TreeUnaryOp,
        TreeBinaryOp,
        TreeConstant,
        TreeOracle,
        TreeInvalid>;

/*  Returns a key suitable for use in maps */
using TreeUnaryKey = std::tuple<Opcode::Opcode, const TreeData*>;
using TreeBinaryKey = std::tuple<
    Opcode::Opcode, const TreeData*, const TreeData*>;
using TreeOracleKey = const OracleClause*;
using TreeDataKeyVariant = std::variant<
    bool,               // Used for NaN (true) and invalid (false)
    float,              // Float constants other than NaN
    Opcode::Opcode,     // Nonary operations, other than VAR_FREE
    TreeUnaryKey,       // Unary operations and VAR_FREE
    TreeBinaryKey,      // Binary operations
    TreeOracleKey>;     // Oracles, keyed by their unique_ptr

// Wrapper struct so that we can use forward declarations
struct TreeDataKey : public TreeDataKeyVariant
{
    TreeDataKey(TreeDataKeyVariant&& v)
        : TreeDataKeyVariant(std::move(v))
    { /* Nothing to do here */ }
};

// Wrapper struct around the variant, required to make recursive
// variant structure work out nicely (since TreeData has to be
// forward-declared, which isn't possible with the raw variant).
struct TreeData : public TreeDataVariant,
                         std::enable_shared_from_this<TreeData>
{
    TreeData(TreeDataVariant&& v)
        : TreeDataVariant(std::move(v))
    { /* Nothing to do here */ }

    /*  Returns the opcode of this clause */
    Opcode::Opcode op() const;

    /*  Returns the floating-point value if this is a constant,
     *  throwing an exception otherwise. */
    float value() const;
    struct ValueException : public std::exception {
        const char* what() const throw () override {
            return "Accessed value of non-constant Tree";
        }
    };

    /*  Returns left and right-hand Tree references.
     *  Throws a ChildException if the requested branch is missing. */
    const Tree& lhs() const;
    const Tree& rhs() const;
    struct ChildException : public std::exception {
        const char* what() const throw () override {
            return "Accessed missing child";
        }
    };

    /*  Returns the underlying OracleClause.  If this isn't a TreeOracle,
     *  throws an OracleException. */
    const OracleClause& oracle_clause() const;

    /*  Returns a freshly-baked Oracle from the given clause.
     *  If this isn't a TreeOracle, throws an exception. */
    std::unique_ptr<Oracle> build_oracle() const;

    struct OracleException : public std::exception {
        const char* what() const throw () {
            return "Accessed oracle of non-constant Tree";
        }
    };

    using Key = TreeDataKey;
    Key key() const;
};

}   // namespace libfive
