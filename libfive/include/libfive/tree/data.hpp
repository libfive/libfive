/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/tree/tree.hpp"
#include "libfive/tree/key.hpp"

namespace libfive {

struct TreeNonaryOp {
    Opcode::Opcode op;
};
struct TreeUnaryOp {
    Opcode::Opcode op;
    // This is mutable so that the destructor can swap the tree's lhs
    // out from a variant pointed to by a "const Data*" pointer.
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

/*  TreeData is a wrapper struct around the TreeDataVariant.
 *
 *  It's a wrapper struct to make recursive variants work, since TreeData
 *  had to be forward-declared to be used in Tree.
 *
 *  The struct includes a reference count, to track ownership and destruciton.
 *  This is the same idea as boost::intrusive_ptr, but that doesn't quite
 *  work for our use cases because the top-level Tree destructor wants to use
 *  the heap rather than the stack; otherwise, destroying deep trees can cause
 *  a stack overflow.
 *
 *  For more information on the homebrew shared pointer, see tree.hpp. */
struct TreeData : public TreeDataVariant
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

    mutable std::atomic_uint32_t refcount = 0;
};

}   // namespace libfive
