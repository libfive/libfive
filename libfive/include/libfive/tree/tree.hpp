/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <map>
#include <unordered_map>
#include <variant>
#include <vector>

#include "libfive/tree/opcode.hpp"

// Forward declarations
namespace libfive {
    class Tree;
    struct TreeData;
    struct TreeDataKey;
    class Oracle;
    class OracleClause;

#define TREE_OPERATORS \
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
#define OP_UNARY(OP, C)    libfive::Tree OP(const libfive::Tree& a);
#define OP_BINARY(OP, C)   libfive::Tree OP(const libfive::Tree& a,\
                                                  const libfive::Tree& b);
TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY

/*  Prints the tree to the given ostream. */
std::ostream& operator<<(std::ostream& stream, const libfive::Tree& tree);

////////////////////////////////////////////////////////////////////////////////

/*
 *  A Tree represents a tree of math expressions
 *
 *  It is a data object (passed around by value), which is a zero-cost wrapper
 *  around a shared_ptr<TreeData>
 */
class Tree : public std::shared_ptr<const TreeData> {
public:
    using Data = TreeData;

    // These are the main constructors used to build Trees in code
    // X, Y, and Z are singletons, since they're used a lot
    static Tree X();
    static Tree Y();
    static Tree Z();

    // Returns a tree with OP_INVALID
    static Tree invalid();

    //  Returns a new unique variable
    static Tree var();

    //  Returns a version of this tree wrapped in the CONST_VAR opcode,
    //  which zeroes out partial derivatives with respect to all variables.
    Tree with_const_vars() const;

    // Construct a unary Tree
    // If the operation is idempotent, e.g. abs(abs(...)),
    // returns the previous value.
    static Tree unary(Opcode::Opcode op, const Tree& lhs);

    // Constructs a binary-operation Tree, simplifying arithmetic
    // identities as needed.
    static Tree binary(Opcode::Opcode op,
                       const Tree& lhs,
                       const Tree& rhs);

    // Constructs a zero-argument tree
    static Tree nonary(Opcode::Opcode op);

    // Constructs a constant Tree with a floating-point value
    Tree(float v);

    // Constructs a Tree from an OracleClause
    explicit Tree(std::unique_ptr<const OracleClause>&& oracle);

    bool is_valid() const;

    /*  Unique identifier for the underlying clause.  This is not necessarily
     *  deduplicated, unless the tree was constructed using unique(). */
    using Id = const void*;
    Id id() const { return get(); }

    /*  Performs a deep copy of the tree with any duplicate subtrees merged
     *  to point to the same objects. */
    Tree unique() const;

    /*  This is a helper function which actually does the uniquifying.
     *  It's exposed so that it's possible to uniquify multiple trees
     *  together, which is helpful in niche circumstances. */
    Tree unique_helper(std::unordered_map<Id, const Data*>& remap,
                       std::map<TreeDataKey, const Data*>& canonical,
                       std::vector<Tree>& new_trees) const;

    /*  Recurses through the graph, accumulating the affine form of child nodes
     *  into a map of t1*a + t2*b + t3*c... */
    using AffinePair = std::pair<Tree, float>;
    using AffineMap = std::unordered_map<Tree::Id, std::vector<AffinePair>>;
    void explore_affine(AffineMap& map,
                        std::unordered_map<const Data*, float>* prev,
                        float scale) const;

    /*  Returns a tree in which nested affine forms are collapsed, e.g.
     *  (2*X + 3*Y) + 5*(X - Y) ==> 7*X - 2*Y   */
    Tree collect_affine() const;

    /*  Checks the number of unique nodes in the tree */
    size_t size() const;

    /*  Remaps the coordinates of this tree, returning a new tree.  */
    Tree remap(Tree X, Tree Y, Tree Z) const;

    /*  Generic (static) remapper */
    Tree remap_from(std::unordered_map<Id, Tree> remap) const;

    /*  Serializes the tree to a stream of bytes */
    void serialize(std::ostream& out) const;

    /*  Attempts to deserialize from a stream of bytes.
     *  Returns invalid() on failure. */
    static Tree deserialize(std::istream& in);

    /* Loads a tree from a file */
    static Tree load(const std::string& filename);

    std::vector<const Data*> walk() const;

protected:
    /*  Does a binary reduction of a set of affine pairs, building
     *  a balanced-ish tree with a recursive approach.  The a iterator is the
     *  beginning of the region to reduce, and b is one-past-the-end of the
     *  region to reduce. */
    static Tree reduce_binary(std::vector<AffinePair>::const_iterator a,
                              std::vector<AffinePair>::const_iterator b);

    // Private constructor to build from the raw variant type
    explicit Tree(std::shared_ptr<const Data> d);

    std::ostream& print_prefix(std::ostream& stream) const;

#define OP_UNARY(OP, C)  friend Tree OP(const Tree&);
#define OP_BINARY(OP, C) friend Tree OP(const Tree&,    \
                                              const Tree&);
TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY
    friend struct TreeData;
    friend std::ostream& operator<<(std::ostream& stream,
                                    const libfive::Tree& tree);
};

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
    bool,           // Used for NaN (true) and invalid (false)
    float,          // Float constants
    Opcode::Opcode, // Nonary operations, other than VAR_FREE
    TreeUnaryKey,       // Unary operations and VAR_FREE
    TreeBinaryKey,      // Binary operations
    TreeOracleKey>;     // Oracles, keyed by their unique_ptr

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
    struct ValueException : public std::exception {
        const char* what() const throw () override {
            return "Accessed value of non-constant Tree";
        }
    };
    float value() const;

    /*  Returns left and right-hand Tree references.
     *  Throws a ChildException if the requested branch is missing. */
    struct ChildException : public std::exception {
        const char* what() const throw () override {
            return "Accessed missing child";
        }
    };
    const Tree& lhs() const;
    const Tree& rhs() const;

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

/*
 *  Represents a Tree which has been deduplicated.
 *
 *  This conversion is implicit, so you can construct objects which
 *  expect a OptimizedTree using a Tree, and it will deduplciate
 *  things under the hood.
 */
class OptimizedTree {
public:
    OptimizedTree(const Tree& t) : tree(t.unique()) {}
    Tree tree;
};

}   // namespace libfive

// Needed so that the unique_ptr<const OracleClause> destructor works
#include "libfive/oracle/oracle_clause.hpp"
