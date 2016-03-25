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
#include <glm/mat4x4.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/kernel/tree/token.hpp"
#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"

Token* rectangle(Store* s, float xmin, float xmax, float ymin, float ymax, glm::mat4 M)
{
    auto x = s->affine(M[0][0], M[0][1], M[0][2], M[0][3]);
    auto y = s->affine(M[1][0], M[1][1], M[1][2], M[1][3]);

    return s->operation(OP_MAX,
           s->operation(OP_MAX, s->operation(OP_SUB, s->constant(xmin), x),
                                s->operation(OP_SUB, x, s->constant(xmax))),
           s->operation(OP_MAX, s->operation(OP_SUB, s->constant(ymin), y),
                                s->operation(OP_SUB, y, s->constant(ymax))));
}
Token* recurse(Store* s, float x, float y, float scale, glm::mat4 M, int i)
{
    auto base = rectangle(s, x - scale/2, x + scale/2,
                             y - scale/2, y + scale/2, M);

    if (i == 0)
    {
        return base;
    }
    else
    {
        auto j = i - 1;
        auto t = scale / 3;

        return s->operation(OP_MIN, base,
               s->operation(OP_MIN, recurse(s, x + scale, y, t, M, j),
               s->operation(OP_MIN, recurse(s, x - scale, y, t, M, j),
               s->operation(OP_MIN, recurse(s, x, y + scale, t, M, j),
               s->operation(OP_MIN, recurse(s, x, y - scale, t, M, j),
               s->operation(OP_MIN, recurse(s, x + scale, y + scale, t, M, j),
               s->operation(OP_MIN, recurse(s, x + scale, y - scale, t, M, j),
               s->operation(OP_MIN, recurse(s, x - scale, y + scale, t, M, j),
                                    recurse(s, x - scale, y - scale, t, M, j)
               ))))))));
    }
};

Tree* menger(int i)
{
    Store s;

    auto M = glm::mat4();
    Token* a = recurse(&s, 0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {1, 0, 0});
    Token* b = recurse(&s, 0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {0, 1, 0});
    Token* c = recurse(&s, 0, 0, 1, M, i);

    auto cube = s.operation(OP_MAX,
                s.operation(OP_MAX,
                   s.operation(OP_MAX, s.affine(-1,  0,  0, -1.5),
                                       s.affine( 1,  0,  0, -1.5)),
                   s.operation(OP_MAX, s.affine( 0, -1,  0, -1.5),
                                       s.affine( 0,  1,  0, -1.5))),
                   s.operation(OP_MAX, s.affine( 0,  0, -1, -1.5),
                                       s.affine( 0,  0,  1, -1.5)));

    Token* cutout = s.operation(OP_NEG,
                    s.operation(OP_MIN, s.operation(OP_MIN, a, b), c));

    return new Tree(&s, s.operation(OP_MAX, cube, cutout));
}
