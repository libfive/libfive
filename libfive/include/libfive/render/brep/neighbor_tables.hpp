/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <utility>

#include <boost/container/static_vector.hpp>
#include "libfive/render/brep/indexes.hpp"
#include "libfive/render/brep/util.hpp"

namespace libfive {

template <unsigned N>
class NeighborTables
{
public:
    /*
     *  Returns the corner index (within a particular neighbor) that matches
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
    static constexpr NeighborIndex getNeighbor(
            NeighborIndex subcell, NeighborIndex neighbor)
    {
        // The subcell must be a child of the neighbor
        // (e.g. if this is a face-adjacent neighbor, then the subcell must
        //  be that face, or one of its edges or corners).
        return neighbor.contains(subcell)

            // If that is the case, then toggle the subcell's fixed axes
            ? NeighborIndex::fromPosAndFloating(
                    ((1 << N) - 1) & (subcell.pos() ^ neighbor.fixed<N>()),
                    subcell.floating())

            // Otherwise, they're not compatible
            : -1;
    }

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
     *      - - - - - - |------------
     *      |  2  |  3  < 10  |     |
     *      - - -t20 - -|------------
     *      |  0  |  1  |     |     |
     *       - - - - - -|------------
     */
    static constexpr std::pair<NeighborIndex, CornerIndex>
    pushIndex(CornerIndex c, NeighborIndex n)
    {
        return (withinTreeIndex(c, n).i != -1)
            ? std::make_pair(NeighborIndex(-1), withinTreeIndex(c, n))
            : neighborTargetIndex(c, n);
    }

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
     *  Given an quad/octree child index, returns a child index
     *  for the given neighbor (if it is within the same quad/octree)
     *
     *  For example, in 2D:
     *
     *  Calling withinTreeIndex(0b00, 0t20) should return (0t20, 0b01)
     *  since that's the child index that will form the new neighbor.
     *  ------------|------------
     *  |     :     |     |     |
     *  | - - - -  t20-----------
     *  |     : b01 < b00 |     |
     *  ------------|------------
     *
     *  Calling withinTreeIndex(0b00, 0t10) should return (0t20, 0b11)
     *  since that's the child index that will form the new neighbor.
     *  ------------|------------
     *  |     : b11 |     |     |
     *  | - - - -  -X------------
     *  |     :     | b00 |     |
     *  ------------|------------
     *
     *  If the corner isn't present on the neighbor, returns nonsense
     */
    static constexpr std::pair<NeighborIndex, CornerIndex>
    neighborTargetIndex(CornerIndex child, NeighborIndex neighbor)
    {
        return std::make_pair(
            whichNeighbor(child, neighbor),
            ((1 << N) - 1) & ((child.i ^ neighbor.fixed()) | (child.i & neighbor.floating())));
    }

    /*
     *  For a given quad/octree quadrant and neighbor index, return the
     *  index of the neighbor which will end up being our new neighbor
     *  when we push.
     *
     *  This is only valid if the pair has a meaningful solution.
     *
     *  Here are the examples I worked through to derive this rule:
     *
     *  0b00 + 0t00 -> 0t00
     *  0b00 + 0t01 -> 0t02
     *  0b00 + 0t02 -> 0t02
     *  0b00 + 0t10 -> 0t20
     *  0b00 + 0b11 -> invalid (0t22)
     *  0b00 + 0t20 -> 0t20
     *  0b00 + 0b21 -> invalid (0t22)
     *  0b00 + 0b22 -> invalid (0t22)
     *
     *  0b01 + 0t00 -> 0t02
     *  0b01 + 0t01 -> 0t01
     *  0b01 + 0t02 -> 0t02
     *  0b01 + 0t10 -> invalid (0t22)
     *  0b01 + 0t11 -> 0t21
     *  0b01 + 0t12 -> invalid (0t22)
     *  0b01 + 0t20 -> invalid (0t22)
     *  0b01 + 0t21 -> 0t21
     *  0b01 + 0t22 -> invalid (0t22)
     *
     *  Digit rule:
     *     t0   1   2
     *  b0  0   2   2
     *   1  2   1   2
     *
     *  Don't worry about it.
     */
    static constexpr NeighborIndex whichNeighbor(CornerIndex c, NeighborIndex n)
    {
#define ci (c.i & 1)
#define ni (n.i % 3)
        return NeighborIndex(
            (c.i || n.i)
            ? (ci == ni ? ci : 2) +
              3 * whichNeighbor(c.i >> 1, n.i / 3).i
            : 0);
#undef ci
#undef ni
    }

    /*  Pre-calculated version of pushIndex(c, n)
     *  (because compilers don't quite fold the constexpr far enough)
     *
     *  Index it with a particular quad/octant index, and it will return a
     *  list of neighbors + quad/octant indices that form the new neighbors
     *  array.
     */
    using PushIndexArray = std::array<std::pair<NeighborIndex, CornerIndex>,
                                      ipow(3, N) - 1>;
    std::array<PushIndexArray, ipow(2, N)> pushIndexTable_data;
    static const PushIndexArray& pushIndexTable(CornerIndex c);

    /*  Pre-calculated, expanded version of getCorner.
     *
     *  Index it with a particular corner, and it will return a list of
     *  neighbor + corner indices that represent that same corner.
     */
    using CornerTableArray = std::array<std::pair<NeighborIndex, CornerIndex>,
                                        ipow(2, N) - 1>;
    static const CornerTableArray& cornerTable(CornerIndex c);

    /*  Pre-calculated, expanded version of getCorner.
     *
     *  Index it with a particular subspace, and it will return a list of
     *  neighbor + subspace (neighbor) indices that represent that subspace. */
    using NeighborTableVec = boost::container::static_vector<
            std::pair<NeighborIndex, NeighborIndex>,
            ipow(2, N)>;
    static const NeighborTableVec& neighborTable(NeighborIndex n);

    /*  When collecting QEFs from a set of children, this table contains
     *  the list of which children + subspaces to sum for each subspace
     *  in the parent.  */
    using QEFTableVec = boost::container::static_vector<
            std::pair<CornerIndex, NeighborIndex>,
            ipow(3, N)>;
    static const QEFTableVec& qefSumTable(NeighborIndex n);

protected:
    /*  Implements a Meyer's singleton */
    static const NeighborTables& instance();
    NeighborTables();

    std::array<CornerTableArray, ipow(2, N)> cornerTable_data;
    std::array<NeighborTableVec, ipow(3, N)> neighborTable_data;
    std::array<QEFTableVec, ipow(3, N)> qefSumTable_data;
};

extern template class NeighborTables<2u>;
extern template class NeighborTables<3u>;

}   // namespace libfive
