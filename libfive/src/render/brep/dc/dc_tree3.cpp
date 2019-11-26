/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "dc_tree.inl"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////
// Specializations for octree
template <>
bool DCTree<3>::leafsAreManifold(
        const std::array<DCTree<3>*, 1 << 3>& cs,
        const std::array<Interval::State, 1 << 3>& corners)
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
        (cs[0]->cornerState(Axis::Z) == corners[0] ||
         cs[0]->cornerState(Axis::Z) == corners[Axis::Z])
    &&  (cs[0]->cornerState(Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::X) == corners[Axis::X])
    &&  (cs[0]->cornerState(Axis::Y) == corners[0] ||
         cs[0]->cornerState(Axis::Y) == corners[Axis::Y])

    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X|Axis::Y])
    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Z) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Z) == corners[Axis::X|Axis::Z])

    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X])
    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::Z) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::Z) == corners[Axis::Y|Axis::Z])

    &&  (cs[Axis::X|Axis::Y]->cornerState(Axis::X|Axis::Y|Axis::Z) ==
                               corners[Axis::X|Axis::Y] ||
         cs[Axis::X|Axis::Y]->cornerState(Axis::X|Axis::Y|Axis::Z) ==
                               corners[Axis::X|Axis::Y|Axis::Z])

    &&  (cs[Axis::Z]->cornerState(Axis::Z|Axis::X) == corners[Axis::Z] ||
         cs[Axis::Z]->cornerState(Axis::Z|Axis::X) == corners[Axis::Z|Axis::X])
    &&  (cs[Axis::Z]->cornerState(Axis::Z|Axis::Y) == corners[Axis::Z] ||
         cs[Axis::Z]->cornerState(Axis::Z|Axis::Y) == corners[Axis::Z|Axis::Y])

    &&  (cs[Axis::Z|Axis::X]->cornerState(Axis::Z|Axis::X|Axis::Y) ==
                               corners[Axis::Z|Axis::X] ||
         cs[Axis::Z|Axis::X]->cornerState(Axis::Z|Axis::X|Axis::Y) ==
                               corners[Axis::Z|Axis::X|Axis::Y])

    &&  (cs[Axis::Z|Axis::Y]->cornerState(Axis::Z|Axis::Y|Axis::X) ==
                               corners[Axis::Z|Axis::Y] ||
         cs[Axis::Z|Axis::Y]->cornerState(Axis::Z|Axis::Y|Axis::X) ==
                               corners[Axis::Z|Axis::Y|Axis::X]);

    const bool faces_safe =
        (cs[0]->cornerState(Axis::X|Axis::Z) == corners[0] ||
         cs[0]->cornerState(Axis::X|Axis::Z) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::X|Axis::Z) == corners[Axis::Z] ||
         cs[0]->cornerState(Axis::X|Axis::Z) == corners[Axis::X|Axis::Z])
    &&  (cs[0]->cornerState(Axis::Y|Axis::Z) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::Z) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::Z) == corners[Axis::Z] ||
         cs[0]->cornerState(Axis::Y|Axis::Z) == corners[Axis::Y|Axis::Z])
    &&  (cs[0]->cornerState(Axis::Y|Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X])

    && (cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::X) == corners[Axis::X] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::X) == corners[Axis::X|Axis::Z] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::X) == corners[Axis::X|Axis::Y] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::X) ==
                                     corners[Axis::X|Axis::Y|Axis::Z])
    && (cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Y) == corners[Axis::Y] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Y) == corners[Axis::Y|Axis::Z] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Y) == corners[Axis::Y|Axis::X] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Y) ==
                                     corners[Axis::Y|Axis::Z|Axis::X])
    && (cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Z) == corners[Axis::Z] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Z) == corners[Axis::Z|Axis::Y] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Z) == corners[Axis::Z|Axis::X] ||
        cs[Axis::X|Axis::Y|Axis::Z]->cornerState(Axis::Z) ==
                                     corners[Axis::Z|Axis::Y|Axis::X]);

    const bool center_safe =
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[0] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::X] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::Y] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::X|Axis::Y] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::Z] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::Z|Axis::X] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::Z|Axis::Y] ||
        cs[0]->cornerState(Axis::X|Axis::Y|Axis::Z) == corners[Axis::Z|Axis::X|Axis::Y];

    return edges_safe && faces_safe && center_safe;
}

template <>
bool DCTree<3>::cornersAreManifold(const uint8_t corner_mask)
{
    /* The code to generate the table is given below:
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
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,
         1,0,1,1,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,1,
         1,0,0,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,
         1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

// Explicit initialization of template
template class DCTree<3>;
template struct DCLeaf<3>;

}   // namespace libfive
