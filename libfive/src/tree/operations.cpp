/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/tree/tree.hpp"
#include "libfive/tree/data.hpp"
#include "libfive/tree/operations.hpp"

namespace libfive {

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode)                      \
Tree name(const Tree& lhs) {                        \
    return Tree::unary(Opcode::opcode, lhs);        \
}
#define OP_BINARY(name, opcode)                     \
Tree name(const Tree& lhs, const Tree& rhs) {       \
    return Tree::binary(Opcode::opcode, lhs, rhs);  \
}
LIBFIVE_TREE_OPERATORS

Tree pow(const Tree& lhs, const Tree& rhs) {
    if (auto i = std::get_if<TreeConstant>(rhs.get())) {
        if (i->value != int(i->value)) {
            throw TreeData::PowValueException();
        }
        return Tree::binary(Opcode::OP_POW, lhs, rhs);
    } else {
        throw TreeData::PowValueException();
    }
}

Tree nth_root(const Tree& lhs, const Tree& rhs) {
    if (auto i = std::get_if<TreeConstant>(rhs.get())) {
        if (i->value != int(i->value)) {
            throw TreeData::PowValueException();
        }
        return Tree::binary(Opcode::OP_NTH_ROOT, lhs, rhs);
    } else {
        throw TreeData::PowValueException();
    }
}

std::ostream& operator<<(std::ostream& stream, const Tree& tree)
{
    return tree.print_prefix(stream);
}

}   // namespace libfive
