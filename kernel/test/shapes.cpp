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

Token* rectangle(float xmin, float xmax, float ymin, float ymax, glm::mat4 M)
{
    auto x = Token::affine(M[0][0], M[0][1], M[0][2], M[0][3]);
    auto y = Token::affine(M[1][0], M[1][1], M[1][2], M[1][3]);

    return Token::operation(Opcode::MAX,
               Token::operation(Opcode::MAX,
                   Token::operation(Opcode::SUB, Token::constant(xmin), x),
                   Token::operation(Opcode::SUB, x, Token::constant(xmax))),
               Token::operation(Opcode::MAX,
                   Token::operation(Opcode::SUB, Token::constant(ymin), y),
                   Token::operation(Opcode::SUB, y, Token::constant(ymax))));
}

Token* recurse(float x, float y, float scale, glm::mat4 M, int i)
{
    auto base = rectangle(x - scale/2, x + scale/2,
                          y - scale/2, y + scale/2, M);

    if (i == 0)
    {
        return base;
    }
    else
    {
        auto j = i - 1;
        auto t = scale / 3;

        return Token::operation(Opcode::MIN, base,
               Token::operation(Opcode::MIN, recurse(x + scale, y, t, M, j),
               Token::operation(Opcode::MIN, recurse(x - scale, y, t, M, j),
               Token::operation(Opcode::MIN, recurse(x, y + scale, t, M, j),
               Token::operation(Opcode::MIN, recurse(x, y - scale, t, M, j),
               Token::operation(Opcode::MIN, recurse(x + scale, y + scale, t, M, j),
               Token::operation(Opcode::MIN, recurse(x + scale, y - scale, t, M, j),
               Token::operation(Opcode::MIN, recurse(x - scale, y + scale, t, M, j),
                                             recurse(x - scale, y - scale, t, M, j)
               ))))))));
    }
}

Token* menger(int i)
{
    auto M = glm::mat4();
    Token* a = recurse(0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {1, 0, 0});
    Token* b = recurse(0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {0, 1, 0});
    Token* c = recurse(0, 0, 1, M, i);

    auto cube = Token::operation(Opcode::MAX,
                Token::operation(Opcode::MAX,
                   Token::operation(Opcode::MAX, Token::affine(-1,  0,  0, -1.5),
                                       Token::affine( 1,  0,  0, -1.5)),
                   Token::operation(Opcode::MAX, Token::affine( 0, -1,  0, -1.5),
                                       Token::affine( 0,  1,  0, -1.5))),
                   Token::operation(Opcode::MAX, Token::affine( 0,  0, -1, -1.5),
                                       Token::affine( 0,  0,  1, -1.5)));

    auto cutout = Token::operation(Opcode::NEG,
                  Token::operation(Opcode::MIN, Token::operation(Opcode::MIN, a, b), c));

    return Token::operation(Opcode::MAX, cube, cutout);
}
