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

#include "ao/kernel/tree/tree.hpp"

Tree rectangle(float xmin, float xmax, float ymin, float ymax, glm::mat4 M)
{
    auto x = Tree::affine(M[0][0], M[0][1], M[0][2], M[0][3]);
    auto y = Tree::affine(M[1][0], M[1][1], M[1][2], M[1][3]);

    return Tree(Opcode::MAX,
               Tree(Opcode::MAX,
                   Tree(Opcode::SUB, Tree(xmin), x),
                   Tree(Opcode::SUB, x, Tree(xmax))),
               Tree(Opcode::MAX,
                   Tree(Opcode::SUB, Tree(ymin), y),
                   Tree(Opcode::SUB, y, Tree(ymax))));
}

Tree recurse(float x, float y, float scale, glm::mat4 M, int i)
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

        return Tree(Opcode::MIN, base,
               Tree(Opcode::MIN, recurse(x + scale, y, t, M, j),
               Tree(Opcode::MIN, recurse(x - scale, y, t, M, j),
               Tree(Opcode::MIN, recurse(x, y + scale, t, M, j),
               Tree(Opcode::MIN, recurse(x, y - scale, t, M, j),
               Tree(Opcode::MIN, recurse(x + scale, y + scale, t, M, j),
               Tree(Opcode::MIN, recurse(x + scale, y - scale, t, M, j),
               Tree(Opcode::MIN, recurse(x - scale, y + scale, t, M, j),
                                             recurse(x - scale, y - scale, t, M, j)
               ))))))));
    }
}

Tree menger(int i)
{
    auto M = glm::mat4();
    Tree a = recurse(0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {1, 0, 0});
    Tree b = recurse(0, 0, 1, M, i);

    M = glm::rotate(M, float(M_PI/2), {0, 1, 0});
    Tree c = recurse(0, 0, 1, M, i);

    auto cube = Tree(Opcode::MAX,
                Tree(Opcode::MAX,
                   Tree(Opcode::MAX, Tree::affine(-1,  0,  0, -1.5),
                                       Tree::affine( 1,  0,  0, -1.5)),
                   Tree(Opcode::MAX, Tree::affine( 0, -1,  0, -1.5),
                                       Tree::affine( 0,  1,  0, -1.5))),
                   Tree(Opcode::MAX, Tree::affine( 0,  0, -1, -1.5),
                                       Tree::affine( 0,  0,  1, -1.5)));

    auto cutout = Tree(Opcode::NEG,
                  Tree(Opcode::MIN, Tree(Opcode::MIN, a, b), c));

    return Tree(Opcode::MAX, cube, cutout);
}

Tree circle(float r)
{
    return Tree(Opcode::SUB,
        Tree(Opcode::ADD,
            Tree(Opcode::MUL, Tree::X(), Tree::X()),
            Tree(Opcode::MUL, Tree::Y(), Tree::Y())),
        Tree(pow(r, 2)));
}

Tree sphere(float r)
{
    return Tree(Opcode::SUB,
        Tree(Opcode::ADD,
        Tree(Opcode::ADD,
            Tree(Opcode::MUL, Tree::X(), Tree::X()),
            Tree(Opcode::MUL, Tree::Y(), Tree::Y())),
            Tree(Opcode::MUL, Tree::Z(), Tree::Z())),
        Tree(pow(r, 2)));
}
