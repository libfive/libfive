/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <memory>
#include <list>
#include <vector>
#include <map>

#include "libfive/tree/opcode.hpp"
#include "libfive/tree/oracle_clause.hpp"

namespace Kernel {

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
    /*
     *  Returns a Tree for the given constant
     */
    Tree(float v);

    /*
     *  Returns a Tree for the given oracle, taking ownership
     */
    Tree(std::unique_ptr<const OracleClause> oracle);

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

    /*  Bitfield enum for node flags */
    enum Flags {
        /*  Does this Id only contain constants and variables
         *  (no VAR_X, VAR_Y, VAR_Z, or ORACLE opcodes allowed) */
        FLAG_LOCATION_AGNOSTIC  = (1<<1),
    };

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

        /*
         *  Pushes a Scheme-format serialization to an ostream
         */
        void print(std::ostream& stream,
                   Opcode::Opcode prev_op=Opcode::INVALID);
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
    Tree remap(std::map<Id, std::shared_ptr<Tree_>> m) const;

    /*
     *  Walks the tree in rank order, from lowest to highest
     *  The last item in the list will be the tree this is called on
     */
    std::list<Tree> ordered() const;

    /*
     *  Serializes to a vector of bytes
     */
    std::vector<uint8_t> serialize() const;

    /*
     *  Deserialize a tree from a set of bytes
     */
    static Tree deserialize(const std::vector<uint8_t>& data);

    /*
     *  Loads a tree from a file
     */
    static Tree load(const std::string& filename);

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

}   // namespace Kernel

// Mass-produce declarations for overloaded operations
#define OP_UNARY(OP)      Kernel::Tree OP(const Kernel::Tree& a)
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

#define OP_BINARY(OP)     Kernel::Tree OP(const Kernel::Tree& a, const Kernel::Tree& b)
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
std::ostream& operator<<(std::ostream& stream, const Kernel::Tree& tree);
