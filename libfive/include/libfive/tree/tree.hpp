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
 *  It is a data object (passed around by value), which is a reference-counted
 *  wrapper around a TreeData pointer on the heap.  This is a homebrew shared
 *  pointer class, because we have very particular needs:
 *  - For the C API, we want to release pointers without decrementing the
 *    reference count, which prohibits shared_ptr.
 *  - When destroying deeply nested trees, we want to use the heap rather than
 *    the stack to prevent overflow, which prohibits boost::intrusive_ptr.
 *
 *  This means that we need a full-on rule-of-five class, with all of the
 *  excitement that brings along with it.
 */
class Tree {
public:
    using Data = TreeData;

    ~Tree();
    Tree(const Tree& other)
        : Tree(other.ptr)
    { /* Nothing to do here */ }
    Tree(Tree&& other) noexcept
        : ptr(std::exchange(other.ptr, nullptr))
    { /* Nothing to do here */ }
    Tree& operator=(const Tree& other) {
        return *this = Tree(other.ptr);
    }
    Tree& operator=(Tree&& other) noexcept {
        std::swap(ptr, other.ptr);
        return *this;
    }

    /* Constructor to build from the raw variant pointer.  This is only useful
     * for building a temporary Tree around a pointer acquired from release() */
    explicit Tree(const Data* d)
        : Tree(d, true)
    { /* Nothing to do here */ }

    // These are the main constructors used to build Trees in code
    // X, Y, and Z are singletons, since they're used a lot
    static Tree X();
    static Tree Y();
    static Tree Z();

    // Returns a tree for which is_invalid() = true
    // (using the TreeInvalid variant)
    static Tree invalid();

    //  Returns a new unique variable
    static Tree var();

    // Compares two trees by *address*, not value.
    // This is O(1), but isn't a deep comparison unless you've deduplicated
    // the two trees using the same canonical map.
    bool operator==(const Tree& other) const;
    bool operator!=(const Tree& other) const;
    bool operator<(const Tree& other) const;

    //  Returns a version of this tree wrapped in the CONST_VAR opcode,
    //  which zeroes out partial derivatives with respect to all variables.
    Tree with_const_vars() const;

    // Construct a unary Tree, applying local simplifications as appropriate
    // e.g. abs(abs(t)) --> abs(t)
    static Tree unary(Opcode::Opcode op, const Tree& lhs);

    // Constructs a binary-operation Tree, apply local simplifications as
    // appropriate (e.g. t + 0 --> t)
    static Tree binary(Opcode::Opcode op, const Tree& lhs, const Tree& rhs);

    // Constructs a zero-argument tree.  If the opcode is VAR_X/Y/Z, returns
    // the singleton X/Y/Z objects from the functions above.
    static Tree nonary(Opcode::Opcode op);

    // Constructs a constant Tree with a floating-point value
    Tree(float v);

    // Constructs a Tree from an OracleClause
    explicit Tree(std::unique_ptr<const OracleClause>&& oracle);

    // Checks whether this tree was constructed by Tree::invalid()
    bool is_valid() const;

    /*  Unique identifier for the underlying clause.  This is not necessarily
     *  deduplicated, unless the tree was constructed using unique(). */
    using Id = const void*;
    Id id() const { return ptr; }

    /*  Returns a new tree which has been unique-ified and has had its affine
     *  subtrees collapsed + balanced. */
    Tree optimized() const;
    Tree optimized_helper(std::map<TreeDataKey, Tree>& canonical) const;

    /*  Returns a tree with all remap operations expanded. */
    Tree flatten() const;

    /*  Performs a deep copy of the tree with any duplicate subtrees merged
     *  to point to the same objects.  This will also flatten any remap
     *  operations in the tree. */
    Tree unique() const;

    /*  This is a helper function which actually does the uniquifying.
     *  It's exposed so that it's possible to uniquify multiple trees
     *  together, which is helpful in niche circumstances. */
    Tree unique_helper(std::map<TreeDataKey, Tree>& canonical) const;

    /*  Recurses through the graph, accumulating the affine form of child nodes
     *  into a map of t1*a + t2*b + t3*c...
     *
     *  The tree must be flattened, otherwise a TreeData::RemapException
     *  will be thrown upon encountering a TreeRemap clause. */
    using AffinePair = std::pair<Tree, float>;
    using AffineMap = std::unordered_map<Tree::Id, std::vector<AffinePair>>;
    AffineMap explore_affine() const;

    /*  Returns a tree in which nested affine forms are collapsed, e.g.
     *  (2*X + 3*Y) + 5*(X - Y) ==> 7*X - 2*Y
     *
     *  If the tree contains remap operations, it will be flattened before
     *  doing the affine collection operation. */
    Tree collect_affine() const;

    /*  Checks the number of unique nodes in the tree */
    size_t size() const;

    /*  Remaps the coordinates of this tree, returning a new tree.
     *
     *  This is normally a constant-time operation, but if X/Y/Z contain
     *  unflattened remap operations of their own, they will be flattened,
     *  which is not constant time. */
    Tree remap(Tree X, Tree Y, Tree Z) const;

    /*  Generic (static) substitution, which does not recurse through oracles.
     *
     *  Throws a RemapException if the tree has unflatten remap operations. */
    Tree substitute(std::unordered_map<Id, Tree>&& s) const;

    /*  Serializes the tree to a stream of bytes */
    void serialize(std::ostream& out) const;

    /*  Attempts to deserialize from a stream of bytes.
     *  Returns invalid() on failure. */
    static Tree deserialize(std::istream& in);

    /* Loads a tree from a file */
    static Tree load(const std::string& filename);

    /*  Performs a leaves-to-root traverse of the tree.
     *
     *  If the tree contains remap operations, it will be flattened
     *  prior to walking. */
    std::vector<const Data*> walk() const;

    /*  Equivalent to shared_ptr::get */
    const Data* get() const { return ptr; }

    /*  Dereferencing follows the pointer */
    const Data* operator->() const { return ptr; }

    /*  Releases the internal pointer (replacing it with nullptr), without
     *  decrementing its reference count.  This is used in the C API to pass
     *  around raw Data pointers.
     *
     *  The pointer must be reclaimed eventually with Tree::reclaim; otherwise,
     *  it will be a memory leak. */
    const Data* release();
    static Tree reclaim(const Data* ptr);

protected:
    /*  This is the managed pointer.  It's mutable so that the destructor
     *  can swap it out for nullptr when flattening out destruction of a
     *  Tree (to avoid blowing up the stack). */
    const Data mutable* ptr = nullptr;

    /*  Does a binary reduction of a set of affine pairs, building a
     *  balanced-ish tree.  The a iterator is the beginning of the region to
     *  reduce, and b is one-past-the-end of the region to reduce. */
    static Tree reduce_binary(std::vector<AffinePair>::const_iterator a,
                              std::vector<AffinePair>::const_iterator b);

    /* Private constructor to build from the raw variant pointer */
    explicit Tree(const Data* d, bool increment_refcount);

    std::ostream& print_prefix(std::ostream& stream) const;

#define OP_UNARY(OP, C)  friend Tree OP(const Tree&);
#define OP_BINARY(OP, C) friend Tree OP(const Tree&, const Tree&);
TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY
    friend std::ostream& operator<<(std::ostream& stream,
                                    const libfive::Tree& tree);
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
    OptimizedTree(const Tree& t) : tree(t.optimized()) {}
    Tree tree;
protected:
    /* Private constructor for special cases where you need to construct
     * an OptimizedTree without actually calling optimized().  You probably
     * shouldn't use this.  The only use case is when reducing constant
     * operations, where we want to construct an evaluator without
     * optimizing the tree (which would recurse). */
    OptimizedTree()
        : tree(Tree::invalid())
    { }
    friend class Tree; // so that Tree can use the special constructor
};

}   // namespace libfive

// Needed so that the unique_ptr<const OracleClause> destructor works
#include "libfive/oracle/oracle_clause.hpp"
