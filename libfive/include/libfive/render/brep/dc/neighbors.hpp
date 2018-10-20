/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>

#include "libfive/render/brep/dc/marching.hpp"
#include "libfive/render/brep/dc/intersection.hpp"
#include "libfive/render/brep/ipow.hpp"
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
    Interval::State check(uint8_t corner) const;

    /*
     *  Looks up the given edge to see if it has been calculated by any
     *  of the neighbors, assigning the pointer if that is the case
     *  and setting it to nullptr otherwise.
     */
    std::shared_ptr<IntersectionVec<N>> check(uint8_t a, uint8_t b) const;

    /*
     *  Returns the neighbors of a particular quad/octree child,
     *  given the child's index and the array of other children.
     */
    Neighbors<N> push(uint8_t child,
            const std::array<std::atomic<XTree<N>*>, 1 << N>&
                children);

protected:
    std::array<const XTree<N>*, ipow(3, N) - 1> neighbors;
};

//  We explicitly instantiate the Neighbors classes in neighbors.cpp
extern template class Neighbors<2>;
extern template class Neighbors<3>;

}   // namespace Kernel
