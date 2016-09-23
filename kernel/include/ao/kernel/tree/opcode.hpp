/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <cstdlib>

namespace Opcode
{
enum Opcode
{
    INVALID,

    CONST,
    VAR_X,
    VAR_Y,
    VAR_Z,

    SQUARE,
    SQRT,
    NEG,
    ABS,
    SIN,
    COS,
    TAN,
    ASIN,
    ACOS,
    ATAN,
    EXP,

    ADD,
    MUL,
    MIN,
    MAX,
    SUB,
    DIV,
    ATAN2,
    POW,
    MOD,
    NANFILL,

    /*
     *  Affine trees must have the form
     *              AFFINE
     *             /      \
     *          ADD       ADD
     *        /    \      /      \
     *      MUL    MUL   MUL     C
     *    /   \   /  \  /   \
     *   X    C  Y   C  Z   C
     *  (where X, Y, Z are base coordinates and C is CONST)
     */
    AFFINE_VEC,

    /*
     *  Dummy opcodes used to select the left or right-hand side of a function
     * (used when one of the children is disabled)
     */
    DUMMY_A,
    DUMMY_B,

    LAST_OP,
};

size_t args(Opcode op);
}

