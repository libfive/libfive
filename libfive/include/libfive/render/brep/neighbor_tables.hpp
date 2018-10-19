/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <utility>

#include "libfive/render/brep/types.hpp"

namespace Kernel {

template <unsigned N>
struct NeighborTables
{
    /*
     *  Returns the corner index (within a particular neighbor) that maches
     *  the provided corner index.
     *
     *  For example, if we call it with neighbor = 6 and corner = 0,
     *  it should return 1 (shown in the drawing below):
     *
     *  --------------
     *  | nei | this |
     *  ------X-------
     *
     */
    static constexpr CornerIndex getCorner(
            CornerIndex corner, NeighborIndex neighbor)
    {
        // We start by checking that the neighbor and corner
        // share the same sign along the neighbor's fixed axes
        return (neighbor.pos() & neighbor.fixed()) ==
               (corner.i & neighbor.fixed())

        // If this is the case, then toggle the bits
        // on the fixed axis and keep the bits on the floating axis,
            ? ((corner.i ^ neighbor.fixed<N>()) |
               ( corner.i & neighbor.floating()))

        // Otherwise, they're not compatible
            : -1;
    }

    /*
     *  Does the same operation as getCorner, but with a subcell
     */
    static NeighborIndex getNeighbor(
            NeighborIndex subcell, NeighborIndex neighbor);

    /*
     *  When pushing into a particular child, this function returns
     *  a neighbor + child pair for each specific neighbor in the new
     *  neighbors array.
     *
     *  If the new neighbor should be taken from within the same tree,
     *  then the neighbor index is -1.
     *
     *  Here's a few examples:
     *  pushIndex(0b00, 0t11) -> {-1, 3}
     *      -------------
     *      |     |  3  |
     *      ------X------
     *      |  0  |     |
     *      -------------
     *
     *  pushIndex(0b10, 0t21) -> {-1, 3}
     *      -------------
     *      | 10  >  3  |
     *      -------------
     *      |     |     |
     *      -------------
     *
     *  pushIndex(0b10, 0t20) -> {0t20, 3}
     *      ------------|------------
     *      |  2  |  3  < 10  |     |
     *      -----t20----|------------
     *      |  0  |  1  |     |     |
     *      ------------|------------
     */
    static std::pair<NeighborIndex, CornerIndex>
    pushIndex(CornerIndex child, NeighborIndex neighbor);

////////////////////////////////////////////////////////////////////////////////
//  FUNCTION BELOW ARE IMPLEMENTATION DETAILS USED FOR pushIndex
//  They're not private so that we can still unit test them

    /*
     *  Given an quad/octree child index, returns the child index of the
     *  given neighbor (if it is within the same quad/octree), or -1 otherwise.
     *
     *  For example, in 2D:
     *
     *  -------------
     *  |     |     |
     *  ------X------
     *  |  0  |     |
     *  -------------
     *
     *  Calling withinTreeIndex(0b00, 0t11) should return (0b11, 0t00)
     *  since that's the XTree child index that contains the neighbor 0t11.
     *
     *  If the first argument is 0b00, then the only valid second arguments
     *  are marked with X here:
     *
     *  -------------
     *  |     |     |
     *   --X--X------
     *  |  0  X     |
     *  ------ ------
     *
     *  This is used when pushing into neighbor arrays.
     */
    static constexpr CornerIndex withinTreeIndex(CornerIndex child,
                                                 NeighborIndex neighbor)
    {
        return ((neighbor.pos() ^ child.i) & neighbor.fixed<N>())
                    == neighbor.fixed<N>()
            ? (neighbor.pos() | (neighbor.floating() & child.i))
            : -1;
    }

    /*
     *  Given an XTree child index, returns a pair of
     *      [neighbor index, XTree child index]
     *  for the given neighbor (if it is within the same XTree)
     *
     *  For example, in 2D:
     *
     *  ------------------------
     *  |     :    |     |     |
     *  | - - - -  -------------
     *  |     : !  |  0  |     |
     *  ------------------------
     *
     *  Calling withinTreeIndex(0b00, 0t20) should return {0t20, 0b01}
     *  since that's the neighbor index and child index that will form
     *  the new neighbor.
     */
    static std::pair<NeighborIndex, CornerIndex>
    neighborTargetIndex(CornerIndex child, NeighborIndex neighbor);
};

}   // namespace Kernel
