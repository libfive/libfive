/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <map>
#include <memory>
#include <functional>
#include <unordered_map>
#include <variant>
#include <vector>

#include "libfive/tree/opcode.hpp"
#include "libfive/tree/operations.hpp"

namespace libfive {

// Forward declarations
struct TreeData;
struct TreeDataKey;
class OracleClause;

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
        : Tree(other.ptr, true, other.flags)
    { /* Nothing to do here */ }
    Tree(Tree&& other) noexcept
        : ptr(std::exchange(other.ptr, nullptr)), flags(other.flags)
    { /* Nothing to do here */ }
    Tree& operator=(const Tree& other) {
        return *this = Tree(other.ptr, true, other.flags);
    }
    Tree& operator=(Tree&& other) noexcept {
        std::swap(ptr, other.ptr);
        flags = other.flags;
        return *this;
    }

    /* Constructor to build from the raw variant pointer.  This is used to
     * build a temporary Tree around a raw pointer acquired from release(),
     * in libfive's C API. */
    explicit Tree(const Data* d)
        : Tree(d, true, 0)
    { /* Nothing to do here */ }

    /* These are the main constructors used to build Trees in code
     * X, Y, and Z are singletons, since they're used a lot */
    static Tree X();
    static Tree Y();
    static Tree Z();

    /* Returns a tree for which is_invalid() = true
     * (under the hood, uses the TreeInvalid variant) */
    static Tree invalid();

    /*  Returns a new unique variable */
    static Tree var();

    /*  Deep comparison, which could be O(N*log(N)) */
    bool eq(const Tree& other) const;

    /* Performs a shallow comparison of two trees
     *
     * This is O(1), but isn't a deep (logical) comparison.
     * Tree:eq() performs a deep comparison. */
    bool operator==(const Tree& other) const;
    bool operator!=(const Tree& other) const;
    bool operator<(const Tree& other) const;

    /*  Returns a version of this tree wrapped in the CONST_VAR opcode,
     *  which zeroes out partial derivatives with respect to all variables. */
    Tree with_const_vars() const;

    /* Construct a unary Tree, applying local simplifications as appropriate
     * e.g. abs(abs(t)) --> abs(t) */
    static Tree unary(Opcode::Opcode op, const Tree& lhs);

    /* Constructs a binary-operation Tree, apply local simplifications as
     * appropriate (e.g. t + 0 --> t) */
    static Tree binary(Opcode::Opcode op, const Tree& lhs, const Tree& rhs);

    /* Constructs a zero-argument tree.  If the opcode is VAR_X/Y/Z, returns
     * the singleton X/Y/Z objects; otherwise, returns a fresh pointer. */
    static Tree nonary(Opcode::Opcode op);

    // Constructs a constant Tree with a floating-point value
    Tree(float v);

    // Constructs a Tree from an OracleClause
    explicit Tree(std::unique_ptr<const OracleClause>&& oracle);

    // Checks whether this tree was constructed by Tree::invalid()
    bool is_valid() const;

    /*  Unique identifier for the underlying clause.  This is not automatically
     *  deduplicated, so the same logic trees may have different ids. */
    using Id = const void*;
    Id id() const { return ptr; }

    /*  Returns a new tree which has been optimized!
     *
     *  An optimized tree is deduplicated: subexpressions which are logically
     *  the same become shared, to make evaluation more efficient.
     *
     *  An optimized tree also has nested affine forms collapsed, e.g.
     *  (2*X + 3*Y) + 5*(X - Y) ==> 7*X - 2*Y
     *
     *  If the input tree contained remap operations, it will be flattened
     *  before optimization */
    Tree optimized() const;
    Tree optimized_helper(
            std::unordered_map<TreeDataKey, Tree>& canonical) const;

    /*  Returns a tree with all remap operations expanded. */
    Tree flatten() const;

    /*  Checks the number of unique nodes in the tree */
    size_t size() const;

    /*  Remaps the coordinates of this tree, returning a new tree.
     *
     *  This is a constant-time lazy operation that is expanded during a call
     *  to flatten() or optimized().
     */
    Tree remap(Tree X, Tree Y, Tree Z) const;

    /*  Substitutes a variable within a tree.
     *
     *  This is a constant-time lazy operation that is expanded during a call
     *  to flatten() or optimized().
     *
     *  var must be a tree returned from Tree::var(); otherwise, an
     *  ApplyException will be thrown.
     */
    Tree apply(Tree var, Tree value) const;

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

    /*  These flags are different from the flags within TreeData in
     *  that they are caches of pure functions of the tree data; instead,
     *  they mark information about the history of the tree (e.g.
     *  whether it was returned from optimized()) */
    enum {
        TREE_FLAG_IS_OPTIMIZED=(1<<0),
    };
    uint32_t flags;

    /*  Returns a Tree with the same data but extra flag bits set */
    Tree with_flags(uint32_t extra_flags) const;

    /* Private constructor to build from the raw variant pointer */
    explicit Tree(const Data* d, bool increment_refcount, uint32_t flags);

    /*  Returns a shared Tree with a constant value of one.
     *  This is used when doing affine reduction. */
    static Tree one();

    std::ostream& print_prefix(std::ostream& stream) const;

#define OP_UNARY(OP, C)  friend Tree OP(const Tree&);
#define OP_BINARY(OP, C) friend Tree OP(const Tree&, const Tree&);
LIBFIVE_TREE_OPERATORS
#undef OP_UNARY
#undef OP_BINARY
    friend std::ostream& operator<<(std::ostream& stream,
                                    const libfive::Tree& tree);
};

}   // namespace libfive

/*  Specialize std::hash so that we can use std::unordered_map of Trees */
namespace std {
template <>
struct hash<libfive::Tree> {
    size_t operator()(const libfive::Tree& k) const {
        return hash<const void*>()(k.id());
    }
};
}   // namespace std

// Needed so that the unique_ptr<const OracleClause> destructor works
#include "libfive/oracle/oracle_clause.hpp"
