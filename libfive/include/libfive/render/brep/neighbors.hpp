/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <array>

#include "libfive/render/brep/marching.hpp"
#include "libfive/render/brep/intersection.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

// Forward declaration
template <unsigned N> class XTree;

/*
 *  Hopefully, the bitmask operations are correct and no one ever needs
 *  to debug this file ever.  To generate the bitmasks, I drew out a lot
 *  of pictures of the 2D case, then thought really hard about whether
 *  it generalized to 3D.
 *
 *  There are more details in the unit tests, including numbering out
 *  all of the 2D cases.
 */
template <unsigned N>
class Neighbors
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    Neighbors();

    /*
     *  Returns the XTree corner index that matches the given corner index
     *  in the given neighbor, or -1 if no such match is possible.
     */
    static int cornerCheckIndex(uint8_t corner, uint8_t neighbor);

    /*
     *  Returns the edge (defined as a pair of corner indices) of a particular
     *  neighbor that matches the given edge.  If no such edge exists, returns
     *  {-1, -1}
     */
    static std::pair<int, int> edgeCheckIndex(
           std::pair<int, int> edge, uint8_t neighbor);

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    Interval::State check(uint8_t corner);

    /*
     *  Looks up the given edge to see if it has been calculated by any
     *  of the neighbors, assigning the pointer if that is the case
     *  and setting it to nullptr otherwise.
     */
    const IntersectionVec<N>* check(uint8_t a, uint8_t b);

    /*
     *  Given an XTree child index, returns the XTree child index of the
     *  given neighbor (if it is within the same XTree), or -1 otherwise.
     *
     *  For example, in 2D:
     *
     *  -------------
     *  |     |     |
     *  -------------
     *  |  0  |     |
     *  -------------
     *
     *  Calling withinTreeIndex(0b00, 0t11) should return 0b11,
     *  since that's the XTree child index that contains the neighbor
     *  0t11 (i.e. 11 in ternary).
     */
    static int withinTreeIndex(uint8_t child, uint8_t neighbor);

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
     *  since that's the XTree child index that contains the neighbor
     *  0t11 (i.e. 11 in ternary).
     */
    static std::pair<int, int> neighborTargetIndex(uint8_t child,
                                                   uint8_t neighbor);


    /*
     *  Returns the neighbors of a particular quad/octree child,
     *  given the child's index and the array of other children.
     */
    Neighbors<N> push(uint8_t child,
            const std::array<std::atomic<const XTree<N>*>, 1 << N>&
                children);

protected:
    /*
     *  Populates fixed, floating, and remap arrays; returns true.
     */
    static bool populatePositions();

    static constexpr uint8_t invert(uint8_t in) {
        return (~in) & mask();
    }
    static constexpr uint8_t mask() {
        return (1 << N) - 1;
    }

    /*  bitfield representing direction for non-floating axes
     *  floating axes have their relevant bit set to 0, but you
     *  need to use the floating field to decode */
    static std::array<uint8_t, _pow(3, N) - 1> fixed;

    /*  bitfield representing which axes are floating  */
    static std::array<uint8_t, _pow(3, N) - 1> floating;

    /*  remap[(fixed << N) | floating] returns the index into
     *  the fixed/floating arrays with the given bitfields.  */
    static std::array<uint8_t, 1 << (2 * N)> remap;

    std::array<const XTree<N>*, _pow(3, N) - 1> neighbors;

    /*  Used as a flag to trigger population of the static arrays */
    static bool loaded;
};

//  We explicitly instantiate the Neighbors classes in neighbors.cpp
extern template class Neighbors<2>;
extern template class Neighbors<3>;

}   // namespace Kernel
