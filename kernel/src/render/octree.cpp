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
#include <iostream>
#include <numeric>
#include <set>

#include <glm/geometric.hpp>

#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"

////////////////////////////////////////////////////////////////////////////////
Octree::Octree(const Subregion& r) : XTree(r)
{
    // Nothing to do here
}

Octree::Octree(Evaluator* e, const Subregion& r, uint32_t flags)
    : XTree(e, r, flags)
{
    finalize(e, flags);
}

Octree::Octree(Evaluator* e, const std::array<Octree*, 8>& cs,
       const Subregion& r, uint32_t flags)
    : XTree(cs, r)
{
    finalize(e, flags);
}

// These are the twelve edges of an octree cell
const std::vector<std::pair<unsigned, unsigned>> Octree::_cellEdges =
    {{0, AXIS_X}, {0, AXIS_Y}, {0, AXIS_Z},
     {AXIS_X, AXIS_X|AXIS_Y}, {AXIS_X, AXIS_X|AXIS_Z},
     {AXIS_Y, AXIS_Y|AXIS_X}, {AXIS_Y, AXIS_Y|AXIS_Z},
     {AXIS_X|AXIS_Y, AXIS_X|AXIS_Y|AXIS_Z},
     {AXIS_Z, AXIS_Z|AXIS_X}, {AXIS_Z, AXIS_Z|AXIS_Y},
     {AXIS_Z|AXIS_X, AXIS_Z|AXIS_X|AXIS_Y},
     {AXIS_Z|AXIS_Y, AXIS_Z|AXIS_Y|AXIS_X}};

////////////////////////////////////////////////////////////////////////////////

bool Octree::cornerTopology() const
{
    /* Implements the test from [Gerstner et al, 2000],
     * as described in [Ju et al, 2002].
     *
     * The code to generate the table is given below:

def safe(index):
    f = [(index & (1 << i)) != 0 for i in range(8)]
    edges = [(0,1), (0,2), (2,3), (1,3),
             (4,5), (4,6), (6,7), (5,7),
             (0,4), (2,6), (1,5), (3,7)]

    def merge(a, b):
        merged = [(e[0] if e[0] != a else b,
                   e[1] if e[1] != a else b) for e in edges]
        return [e for e in merged if e[0] != e[1]]

    while True:
        for e in edges:
            if f[e[0]] == f[e[1]]:
                edges = merge(e[0], e[1])
                break
        else:
            break
    s = set(map(lambda t: tuple(sorted(t)),edges))
    return len(s) <= 1

out = ""
for i,s in enumerate([safe(i) for i in range(256)]):
    if out == "": out += "{"
    else: out += ","

    if i and i % 32 == 0:
        out += '\n '

    if s: out += "1"
    else: out += "0"
out += "}"
print(out)

    */
    static std::vector<bool> corner_table =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,
         1,0,1,1,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,1,
         1,0,0,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,
         1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};

    uint8_t index = 0;
    for (uint8_t i=0; i < 8; ++i)
    {
        if (corners[i])
        {
            index |= (1 << i);
        }
    }

    return corner_table[index];
}

bool Octree::leafTopology() const
{
    /*  - The sign in the middle of a coarse edge must agree with the sign of at
     *    least one of the edge’s two endpoints.
     *  - The sign in the middle of a coarse face must agree with the sign of at
     *    least one of the face’s four corners.
     *  - The sign in the middle of a coarse cube must agree with the sign of at
     *    least one of the cube’s eight corners.
     *  [Ju et al, 2002]    */

    // Check the signs in the middle of leaf cell edges
    const bool edges_safe =
        (child(0)->corner(AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_Z) == corner(AXIS_Z))
    &&  (child(0)->corner(AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_X) == corner(AXIS_X))
    &&  (child(0)->corner(AXIS_Y) == corner(0) ||
         child(0)->corner(AXIS_Y) == corner(AXIS_Y))

    &&  (child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X) ||
         child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X|AXIS_Y))
    &&  (child(AXIS_X)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X) ||
         child(AXIS_X)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X|AXIS_Z))

    &&  (child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X))
    &&  (child(AXIS_Y)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
         child(AXIS_Y)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y|AXIS_Z))

    &&  (child(AXIS_X|AXIS_Y)->corner(AXIS_X|AXIS_Y|AXIS_Z) ==
                               corner(AXIS_X|AXIS_Y) ||
         child(AXIS_X|AXIS_Y)->corner(AXIS_X|AXIS_Y|AXIS_Z) ==
                               corner(AXIS_X|AXIS_Y|AXIS_Z))

    &&  (child(AXIS_Z)->corner(AXIS_Z|AXIS_X) == corner(AXIS_Z) ||
         child(AXIS_Z)->corner(AXIS_Z|AXIS_X) == corner(AXIS_Z|AXIS_X))
    &&  (child(AXIS_Z)->corner(AXIS_Z|AXIS_Y) == corner(AXIS_Z) ||
         child(AXIS_Z)->corner(AXIS_Z|AXIS_Y) == corner(AXIS_Z|AXIS_Y))

    &&  (child(AXIS_Z|AXIS_X)->corner(AXIS_Z|AXIS_X|AXIS_Y) ==
                               corner(AXIS_Z|AXIS_X) ||
         child(AXIS_Z|AXIS_X)->corner(AXIS_Z|AXIS_X|AXIS_Y) ==
                               corner(AXIS_Z|AXIS_X|AXIS_Y))

    &&  (child(AXIS_Z|AXIS_Y)->corner(AXIS_Z|AXIS_Y|AXIS_X) ==
                               corner(AXIS_Z|AXIS_Y) ||
         child(AXIS_Z|AXIS_Y)->corner(AXIS_Z|AXIS_Y|AXIS_X) ==
                               corner(AXIS_Z|AXIS_Y|AXIS_X));

    const bool faces_safe =
        (child(0)->corner(AXIS_X|AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_Z) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X|AXIS_Z))
    &&  (child(0)->corner(AXIS_Y|AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Z) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y|AXIS_Z))
    &&  (child(0)->corner(AXIS_Y|AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_X) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X))

    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X|AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X|AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) ==
                                     corner(AXIS_X|AXIS_Y|AXIS_Z))
    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y|AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y|AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) ==
                                     corner(AXIS_Y|AXIS_Z|AXIS_X))
    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z|AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z|AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) ==
                                     corner(AXIS_Z|AXIS_Y|AXIS_X));

    const bool center_safe =
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(0) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_X) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_X|AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_X) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_X|AXIS_Y);

    return edges_safe && faces_safe && center_safe;
}
