/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <variant>

#include "libfive/tree/opcode.hpp"

namespace libfive {
    class SimpleTree;
}

// Mass-produce declarations for overloaded operations
#define OP_UNARY(OP)      libfive::SimpleTree OP(const libfive::SimpleTree& a)
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

#define OP_BINARY(OP) libfive::SimpleTree OP(const libfive::SimpleTree& a,  \
                                             const libfive::SimpleTree& b)
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
namespace libfive {

// Forward declaration
class OracleClause;
class SimpleTree;
struct SimpleTreeData;

struct SimpleNonaryOp {
    Opcode::Opcode op;
};
struct SimpleUnaryOp {
    Opcode::Opcode op;
    std::shared_ptr<const SimpleTreeData> lhs;
};
struct SimpleBinaryOp {
    Opcode::Opcode op;
    std::shared_ptr<const SimpleTreeData> lhs;
    std::shared_ptr<const SimpleTreeData> rhs;
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

    using UnaryKey = std::tuple<Opcode::Opcode, const SimpleTreeData*>;
    using BinaryKey = std::tuple<Opcode::Opcode, const SimpleTreeData*, const SimpleTreeData*>;
    using Key = std::variant<
        bool, // Used for NaN and invalid
        float,
        Opcode::Opcode,
        UnaryKey,
        BinaryKey>;

    /*  Returns a key suitable for use in maps */
    Key key() const;
};

/*
 *  A SimpleTree represents a tree of math expressions
 *
 *  It is a data object (passed around by value)
 */
class SimpleTree
{
public:
    using Data = SimpleTreeData;

    // These are the main constructors used to build SimpleTrees in code
    static SimpleTree X();
    static SimpleTree Y();
    static SimpleTree Z();
    SimpleTree(float v);

    // Secondary constructor to build from the raw variant type
    explicit SimpleTree(std::shared_ptr<const Data> d);

    /*  Overloaded operator */
    SimpleTree operator-() const;

    /*  Looks up the opcode, returning Opcode::INVALID if its invalid */
    Opcode::Opcode op() const;

    /*  lhs and rhs return an invalid SimpleTree if not present */
    SimpleTree lhs() const;
    SimpleTree rhs() const;

    /*  value() returns the constant if this is a constant, and throws
     *  a SimpleTree::Exception otherwise.  This matches the behavior of
     *  std::get if you get an incorrect variant. */
    float value() const;

    /*  Checks whether this SimpleTree is valid. */
    bool is_valid() const;

    /*  Performs a deep copy of the tree, so that it can be modified without
     *  changing the original. */
    SimpleTree clone() const;

    /*  Performs a deep copy of the tree with any duplicate subtrees merged
     *  to point to the same objects. */
    SimpleTree unique() const;

    /*  Checks the number of unique nodes in the tree */
    size_t size() const;

    /*  Remaps the coordinates of this tree, returning a new tree.  */
    SimpleTree remap(SimpleTree X, SimpleTree Y, SimpleTree Z) const;

    struct Exception : public std::exception {
        const char* what() const throw () {
            return "Accessed value of non-constant SimpleTree";
        }
    };

    std::vector<const Data*> walk() const;

protected:
    static SimpleTree invalid();
    std::shared_ptr<const Data> data;

#define FRIEND(OP) friend libfive::SimpleTree (::OP(const libfive::SimpleTree&));
FRIEND(square)
FRIEND(sqrt)
FRIEND(abs)
FRIEND(sin)
FRIEND(cos)
FRIEND(tan)
FRIEND(asin)
FRIEND(acos)
FRIEND(atan)
FRIEND(log)
FRIEND(exp)
#undef FRIEND
#define FRIEND(OP) friend SimpleTree (::OP(const SimpleTree&, const SimpleTree&));
FRIEND(operator+)
FRIEND(operator*)
FRIEND(min)
FRIEND(max)
FRIEND(operator-)
FRIEND(operator/)
FRIEND(atan2)
FRIEND(pow)
FRIEND(nth_root)
FRIEND(mod)
FRIEND(nanfill)
FRIEND(compare)
#undef FRIEND
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
    SimpleUniqueTree(const SimpleTree& t);
protected:
    SimpleTree t;
};

}   // namespace libfive

