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

// Forward declaration
class OracleClause;
class SimpleTree;

struct SimpleNonaryOp {
    Opcode::Opcode op;
};
struct SimpleUnaryOp {
    Opcode::Opcode op;
    std::shared_ptr<SimpleTree> lhs;
};
struct SimpleBinaryOp {
    Opcode::Opcode op;
    std::shared_ptr<SimpleTree> lhs;
    std::shared_ptr<SimpleTree> rhs;
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

/*
 *  A SimpleTree represents a tree of math expressions
 *
 *  It is a data object (passed around by value)
 */
class SimpleTree
{
public:
    // The SimpleTree stores one of these objects
    using Data = std::variant<
        SimpleNonaryOp,
        SimpleUnaryOp,
        SimpleBinaryOp,
        SimpleConstant,
        SimpleOracle,
        SimpleTreeInvalid>;

    static SimpleTree X();
    static SimpleTree Y();
    static SimpleTree Z();

    SimpleTree(float v);
    SimpleTree(Data&& d);

    /*  Overloaded operator */
    SimpleTree operator-() const;

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

    struct Exception : public std::exception {
        const char* what() const throw () {
            return "Accessed value of non-constant SimpleTree";
        }
    };

protected:
    Data data;
};

}   // namespace libfive

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
