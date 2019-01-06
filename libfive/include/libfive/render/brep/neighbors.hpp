/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/neighbor_tables.hpp"

namespace Kernel {

// N = number of dimensions
// T = leaf node type
// C = CRTP template parameter (so push can return the derived type)
template <unsigned N, typename T, typename C>
class Neighbors
{
public:
    /*  Constructor, returning an empty neighbor array
     */
    Neighbors() {
        std::fill(neighbors.begin(), neighbors.end(), nullptr);
    }

    /*
     *  Returns the neighbors of a particular quad/octree child,
     *  given the child's index and the array of other children.
     */
    C push(uint8_t child,
           const std::array<std::atomic<T*>, 1 << N>& children) const
    {
        C out;
        for (unsigned i=0; i < ipow(3, N) - 1; ++i)
        {
            const auto q = NeighborTables<N>::pushIndexTable[child][i];
            if (q.first.i == -1) {
                out.neighbors[i] = children[q.second.i].load();
            } else if (neighbors[q.first.i]) {
                out.neighbors[i] = neighbors[q.first.i]->
                    children[q.second.i].load();
            }
        }

        /*  Special-casing for this particular situation:
         *
         *  -----------------------------------------
         *  |                    |         |        |
         *  |                    |         |        |
         *  |                    |         |        |
         *  |                    --------------------
         *  |                    |         |        |
         *  |                    |    i    |        |
         *  |                    |         |        |
         *  ---------------------X-------------------
         *  |                    |                  |
         *  |                    |                  |
         *  |         N          |                  |
         *  |                    |                  |
         *  |                    |                  |
         *  |                    |                  |
         *  -----------------------------------------
         *
         *  If we're pushing into the cell labelled i, then the
         *  corner-adjacent neighbor at X remains N, even if N isn't
         *  a subdividing cell.
         */
        const auto n = CornerIndex(child).neighbor();
        if (out.neighbors[n.i] == nullptr) {
            out.neighbors[n.i] = neighbors[n.i];
        }
        return out;
    }

protected:
    std::array<const T*, ipow(3, N) - 1> neighbors;
};

}   // namespace Kernel
