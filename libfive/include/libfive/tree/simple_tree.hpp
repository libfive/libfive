/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <variant>

#include "libfive/tree/opcode.hpp"

// Forward declarations
namespace libfive {
    class SimpleTree;
    struct SimpleTreeData;
    class Oracle;
    class OracleClause;

#define SIMPLE_TREE_OPERATORS \
OP_UNARY(square, OP_SQUARE) \
OP_UNARY(sqrt, OP_SQRT) \
OP_UNARY(abs, OP_ABS) \
OP_UNARY(operator-, OP_NEG) \
OP_UNARY(sin, OP_SIN) \
OP_UNARY(cos, OP_COS) \
OP_UNARY(tan, OP_TAN) \
OP_UNARY(asin, OP_ASIN) \
OP_UNARY(acos, OP_ACOS) \
OP_UNARY(atan, OP_ATAN) \
OP_UNARY(log, OP_LOG) \
OP_UNARY(exp, OP_EXP) \
OP_BINARY(operator+, OP_ADD) \
OP_BINARY(operator*, OP_MUL) \
OP_BINARY(min, OP_MIN) \
OP_BINARY(max, OP_MAX) \
OP_BINARY(operator-, OP_SUB) \
OP_BINARY(operator/, OP_DIV) \
OP_BINARY(atan2, OP_ATAN2) \
OP_BINARY(pow, OP_POW) \
OP_BINARY(nth_root, OP_NTH_ROOT) \
OP_BINARY(mod, OP_MOD) \
OP_BINARY(nanfill, OP_NANFILL) \
OP_BINARY(compare, OP_COMPARE)

// Mass-produce declarations for overloaded operations
#define OP_UNARY(OP, C)    libfive::SimpleTree OP(const libfive::SimpleTree& a);
#define OP_BINARY(OP, C)   libfive::SimpleTree OP(const libfive::SimpleTree& a,\
                                                  const libfive::SimpleTree& b);
SIMPLE_TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY

/*  Prints the tree to the given ostream. */
std::ostream& operator<<(std::ostream& stream, const libfive::SimpleTree& tree);

////////////////////////////////////////////////////////////////////////////////

/*
 *  A SimpleTree represents a tree of math expressions
 *
 *  It is a data object (passed around by value), which is a zero-cost wrapper
 *  around a shared_ptr<SimpleTreeData>
 */
class SimpleTree : public std::shared_ptr<const SimpleTreeData> {
public:
    using Data = SimpleTreeData;

    // These are the main constructors used to build SimpleTrees in code
    // X, Y, and Z are singletons, since they're used a lot
    static SimpleTree X();
    static SimpleTree Y();
    static SimpleTree Z();

    //  Returns a new unique variable
    static SimpleTree var();

    //  Returns a version of this tree wrapped in the CONST_VAR opcode,
    //  which zeroes out partial derivatives with respect to all variables.
    SimpleTree with_const_vars() const;

    // Construct a unary SimpleTree
    // If the operation is idempotent, e.g. abs(abs(...)),
    // returns the previous value.
    static SimpleTree unary(Opcode::Opcode op, const SimpleTree& lhs);

    // Constructs a binary-operation SimpleTree, simplifying arithmetic
    // identities as needed.
    static SimpleTree binary(Opcode::Opcode op,
                             const SimpleTree& lhs,
                             const SimpleTree& rhs);

    // Constructs a constant SimpleTree with a floating-point value
    SimpleTree(float v);

    // Constructs a SimpleTree from an OracleClause
    explicit SimpleTree(const std::shared_ptr<OracleClause>& oracle);

    bool is_valid() const;

    /*  Unique identifier for the underlying clause.  This is not necessarily
     *  deduplicated, unless the tree was constructed using unique(). */
    using Id = const void*;
    Id id() const { return get(); }

    /*  Performs a deep copy of the tree with any duplicate subtrees merged
     *  to point to the same objects. */
    SimpleTree unique() const;

    /*  Checks the number of unique nodes in the tree */
    size_t size() const;

    /*  Remaps the coordinates of this tree, returning a new tree.  */
    SimpleTree remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const;

    /*  Serializes the tree to a stream of bytes */
    void serialize(std::ostream& out) const;

    /*  Attempts to deserialize from a stream of bytes.
     *  Returns invalid() on failure. */
    static SimpleTree deserialize(std::istream& in);

    std::vector<const Data*> walk() const;

protected:
    // Private constructor to build from the raw variant type
    explicit SimpleTree(std::shared_ptr<const Data> d);

    std::ostream& print_prefix(std::ostream& stream) const;

    static SimpleTree invalid();

#define OP_UNARY(OP, C)  friend SimpleTree OP(const SimpleTree&);
#define OP_BINARY(OP, C) friend SimpleTree OP(const SimpleTree&,    \
                                              const SimpleTree&);
SIMPLE_TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY
    friend struct SimpleTreeData;
    friend std::ostream& operator<<(std::ostream& stream,
                                    const libfive::SimpleTree& tree);
};

struct SimpleNonaryOp {
    Opcode::Opcode op;
};
struct SimpleUnaryOp {
    Opcode::Opcode op;
    SimpleTree lhs;
};
struct SimpleBinaryOp {
    Opcode::Opcode op;
    SimpleTree lhs;
    SimpleTree rhs;
};
struct SimpleConstant {
    float value;
};
struct SimpleOracle {
    std::shared_ptr<OracleClause> oracle;
};
struct SimpleTreeInvalid {
    // No members
};
using SimpleTreeDataVariant = std::variant<
        SimpleNonaryOp,
        SimpleUnaryOp,
        SimpleBinaryOp,
        SimpleConstant,
        SimpleOracle,
        SimpleTreeInvalid>;

// Wrapper struct around the variant, required to make recursive
// variant structure work out nicely (since SimpleTreeData has to be
// forward-declared, which isn't possible with the raw variant).
struct SimpleTreeData : public SimpleTreeDataVariant,
                        std::enable_shared_from_this<SimpleTreeData>
{
    SimpleTreeData(const SimpleTreeDataVariant& v)
        : SimpleTreeDataVariant(v)
    { /* Nothing to do here */ }

    /*  Returns the opcode of this clause */
    Opcode::Opcode op() const;

    /*  Returns the floating-point value if this is a constant,
     *  throwing an exception otherwise. */
    struct ValueException : public std::exception {
        const char* what() const throw () override {
            return "Accessed value of non-constant SimpleTree";
        }
    };
    float value() const;

    /*  Returns left and right-hand SimpleTree references.
     *  Throws a ChildException if the requested branch is missing. */
    struct ChildException : public std::exception {
        const char* what() const throw () override {
            return "Accessed missing child";
        }
    };
    const SimpleTree& lhs() const;
    const SimpleTree& rhs() const;

    /*  Returns the underlying OracleClause.  If this isn't a SimpleOracle,
     *  throws an OracleException. */
    const OracleClause& oracle_clause() const;

    /*  Returns a freshly-baked Oracle from the given clause.
     *  If this isn't a SimpleOracle, throws an exception. */
    std::unique_ptr<Oracle> build_oracle() const;
    struct OracleException : public std::exception {
        const char* what() const throw () {
            return "Accessed oracle of non-constant SimpleTree";
        }
    };

    /*  Returns a key suitable for use in maps */
    using UnaryKey = std::tuple<Opcode::Opcode, const SimpleTreeData*>;
    using BinaryKey = std::tuple<
        Opcode::Opcode, const SimpleTreeData*, const SimpleTreeData*>;
    using Key = std::variant<
        bool, // Used for NaN and invalid
        float,
        Opcode::Opcode,
        UnaryKey,
        BinaryKey>;

    Key key() const;
};

/*
 *  Represents a SimpleTree which has been deduplicated.
 *
 *  This conversion is implicit, so you can construct objects which
 *  expect a SimpleUniqueTree using a SimpleTree, and it will deduplciate
 *  things under the hood.
 */
class SimpleUniqueTree {
public:
    SimpleUniqueTree(const SimpleTree& t) : tree(t.unique()) {}
    SimpleTree tree;
};

}   // namespace libfive

