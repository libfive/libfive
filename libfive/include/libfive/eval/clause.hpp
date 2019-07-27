/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/tree/opcode.hpp"

namespace libfive {

/*
 *  A clause is used in an Evaluator to evaluate a tree
 */
struct Clause
{
    /*  Opcode for this clause  */
    const Opcode::Opcode op;

    /*  Populated for operators with arguments */
    uint32_t const id;

    /*  a and b cover the range of arguments (inclusive) */
    uint32_t const* const a;
    uint32_t const* const b;
};

}   // namespace libfive
