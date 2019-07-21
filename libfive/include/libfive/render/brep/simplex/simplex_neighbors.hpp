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

#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/indexes.hpp"

namespace libfive {

// Forward declaration
template <unsigned N> class SimplexTree;
template <unsigned N> struct SimplexLeaf;
template <unsigned N> struct SimplexLeafSubspace;

template <unsigned N>
class SimplexNeighbors :
    public Neighbors<N, SimplexTree<N>, SimplexNeighbors<N>>
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    SimplexNeighbors();

    /*
     *  A given subspace may have been allocated multiple times,
     *  depending on the vagaries of multithreaded evaluation.
     *
     *  This function returns the canonical subspace, as a pointer
     *  to a pool-allocated object.  It decides which subspace is
     *  canonical by picking the smallest pointer, which is arbitrary
     *  but unambiguous.
     *
     *  This function also walks down branching neighbors, e.g. to
     *  find the corner C of cell X, it will walk down cell Y to
     *  cell Z (which contains that corner).
     *
     *   -------------------------
     *   |           |           |
     *   |     X     |           |
     *   |           |           |
     *   ------------C------------
     *   |     |  Z  |           |
     *   |-----Y-----|           |
     *   |     |     |           |
     *   -------------------------
     */
    SimplexLeafSubspace<N>* getSubspace(NeighborIndex i) const;

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    std::pair<const SimplexLeaf<N>*, NeighborIndex> check(NeighborIndex i) const;
};

}   // namespace libfive

