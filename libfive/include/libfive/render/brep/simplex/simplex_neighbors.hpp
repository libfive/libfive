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

namespace Kernel {

// Forward declaration
template <unsigned N> class SimplexTree;
template <unsigned N> struct SimplexLeaf;

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
     *  Looks up the given subspace vertex to see if it has already been
     *  assign an index by any of the neighbors, returning the index if
     *  that's the case and 0 otherwise.
     */
    uint64_t getIndex(NeighborIndex i) const;

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    std::pair<const SimplexLeaf<N>*, NeighborIndex> check(NeighborIndex i) const;
};

}   // namespace Kernel

