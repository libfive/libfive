/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <memory>
#include <list>
#include <vector>
#include <map>
#include <functional>

#include "libfive/tree/opcode.hpp"

namespace libfive {

// Forward declaration
class OracleClause;

/*
 *  A Tree represents a tree of math expressions.
 *
 *  A Tree is a lightweight wrapper containing a shared_ptr to a Tree_,
 *  where data is actually stored.  Deduplication is handled by a global
 *  Cache object
 */
class Tree
{
public:

    /*  Simple enum for branch selection */
    enum Direction { LEFT, RIGHT };

    /*  This is where tree data is actually stored  */
    struct Tree_ {
        /*
         *  Destructor erases this Tree from the global Cache
         */
        ~Tree_();

        const Opcode::Opcode op;
        const uint8_t flags;
        const unsigned rank;

        /*  Only populated for constants  */
        const float value;

        /* Only populated for oracles */
        const std::unique_ptr<const OracleClause> oracle;

        /*  Only populated for operations  */
        const std::shared_ptr<Tree_> lhs;
        const std::shared_ptr<Tree_> rhs;

        /*  Programmatic branch lookup */
        const std::shared_ptr<Tree_> branch(Direction d);

        /*
         *  Pushes a Scheme-format serialization to an ostream
         */
        void print(std::ostream& stream,
                   Opcode::Opcode prev_op=Opcode::INVALID);

        /*
         *  Prints an infix-format string to an ostream
         */
        void printInfix(std::ostream& stream);
    };
    /*
     *  Returns a Tree for the given constant
     */
    Tree(float v);

    /*
     *  Returns a Tree for the given oracle, taking ownership.  Can optionally
     *  take a function to call before deleting the resulting Tree; this function
     *  should not actually delete the Tree or deallocate its memory, but must
     *  fulfill all other conditions for the deleter of a shared_ptr.
     */
    Tree(std::unique_ptr<const OracleClause>&& oracle);
    Tree(std::unique_ptr<const OracleClause>&& oracle, 
         std::function<void(const Tree_*)> onDeletion);

    /*
     *  Constructors for individual axes
     */
    static Tree X() { return Tree(Opcode::VAR_X); }
    static Tree Y() { return Tree(Opcode::VAR_Y); }
    static Tree Z() { return Tree(Opcode::VAR_Z); }

    /*
     *  Used to mark a bad parse, among other things
     */
    static Tree Invalid() { return Tree(std::shared_ptr<Tree_>(nullptr)); }

    /*
     *  Returns a token for the given operation
     *
     *  Arguments should be filled in from left to right
     *  (i.e. a must not be null if b is not null)
     *
     *  If the opcode is POW or NTH_ROOT, b must be an integral CONST
     *  (otherwise an assertion will be triggered).
     *  If the opcode is NTH_ROOT, b must be > 0.
     */
    explicit Tree(Opcode::Opcode op, Tree a=Tree(), Tree b=Tree());

    /*
     *  Returns a new unique variable
     */
    static Tree var();

    /*
     *  Destructor to ensure thread-safety while manipulating the Cache
     */
    ~Tree();

    /*  Bitfield enum for node flags */
    enum Flags {
        /*  Does this Id only contain constants and variables
         *  (no VAR_X, VAR_Y, VAR_Z, or ORACLE opcodes allowed) */
        FLAG_LOCATION_AGNOSTIC  = (1<<1),
    };

    /*  Trees are uniquely identified by their Tree_ address, but we don't
     *  want anyone to do anything with that value  */
    typedef const Tree_* Id;

    /*
     *  Overload arrow to get shared Tree_ value
     */
    const std::shared_ptr<Tree_>& operator->() const { return ptr; }

    /*
     *  Comparison operator for trees
     */
    bool operator==(const Tree& other) const {
        return ptr.get() == other.ptr.get();
    }

    /*
     *  Overloaded operators
     */
    Tree operator-() const;

    /*
     *  Unique identity (as the tree pointer)
     */
    Id id() const { return ptr.get(); }

    /*
     *  Remaps the base coordinates
     */
    Tree remap(Tree X, Tree Y, Tree Z) const;

    /*
     *  Executes an arbitrary remapping
     */
    Tree remap(std::map<Id, Tree> m) const;

    /*
     *  Returns a tree in which all VAR clauses are wrapped in a
     *  CONST_VAR clause.  This effectively disables their contribution
     *  to Jacobian (per-variable gradient) evaluation, which is useful
     *  for making direct modeling more nuanced.
     */
    Tree makeVarsConstant() const;

    /*
     *  Walks the tree in rank order, from lowest to highest
     *  The last item in the list will be the tree this is called on
     */
    std::list<Tree> ordered() const;

    /*
     *  Walks the tree as a depth-first search, then reverses the results
     *  The last item in the list will be the tree this is called on
     */
    std::vector<Tree> orderedDfs() const;

    void serialize(std::ostream& out) const;
    static Tree deserialize(std::istream& in);

    /*
     *  Loads a tree from a file
     */
    static Tree load(const std::string& filename);

    /*
     *  Returns the left-hand and right-hand inputs as trees; if the input
     *  is invalid, returns the same result as Invalid()
     */
    Tree lhs() const;
    Tree rhs() const;

protected:
    /*
     *  Empty tree constructor
     */
    explicit Tree();

    /*
     *  Private constructor
     */
    explicit Tree(std::shared_ptr<Tree_> t) : ptr(t) {}

    /*  Here's the actual Tree data */
    std::shared_ptr<Tree_> ptr;

    /*  These classes need access to private constructor  */
    friend class Cache;
};

}   // namespace libfive

// Mass-produce declarations for overloaded operations
#define OP_UNARY(OP)      libfive::Tree OP(const libfive::Tree& a)
OP_UNARY(square);
OP_UNARY(sqrt);
OP_UNARY(abs);
OP_UNARY(sin);
OP_UNARY(cos);
OP_UNARY(tan);
OP_UNARY(asin);
OP_UNARY(acos);
OP_UNARY(atan);
OP_UNARY(log);
OP_UNARY(exp);
#undef OP_UNARY

#define OP_BINARY(OP)     libfive::Tree OP(const libfive::Tree& a, const libfive::Tree& b)
OP_BINARY(operator+);
OP_BINARY(operator*);
OP_BINARY(min);
OP_BINARY(max);
OP_BINARY(operator-);
OP_BINARY(operator/);
OP_BINARY(atan2);
OP_BINARY(pow);
OP_BINARY(nth_root);
OP_BINARY(mod);
OP_BINARY(nanfill);
OP_BINARY(compare);
#undef OP_BINARY

/*
 *  Deserialize with Scheme-style syntax
 */
std::ostream& operator<<(std::ostream& stream, const libfive::Tree& tree);

// This include goes at the bottom to work around circular ordering
#include "libfive/oracle/oracle_clause.hpp"
