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

enum Opcode
{
    INVALID,

    OP_CONST,
    OP_X,
    OP_Y,
    OP_Z,

    OP_SQUARE,
    OP_SQRT,
    OP_NEG,
    OP_ABS,
    OP_SIN,
    OP_COS,
    OP_TAN,
    OP_ASIN,
    OP_ACOS,
    OP_ATAN,
    OP_EXP,

    OP_ADD,
    OP_MUL,
    OP_MIN,
    OP_MAX,
    OP_SUB,
    OP_DIV,
    OP_ATAN2,
    OP_MOD,
    OP_NANFILL,

    // Affine trees must have the form
    //              AFFINE
    //             /      \
    //          OP_ADD    OP_ADD
    //        /    \      /      \
    //      MUL    MUL   MUL     C
    //    /   \   /  \  /   \
    //   X    C  Y   C  Z   C
    // (where X, Y, Z are base coordinates and C is OP_CONST)
    AFFINE,

    // Dummy opcodes used to select the left or right-hand side of a function
    // (used when one of the children is disabled)
    OP_A,
    OP_B,

    LAST_OP,
};
