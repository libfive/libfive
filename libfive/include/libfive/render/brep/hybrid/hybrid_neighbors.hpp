/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>

#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"
#include "libfive/render/brep/indexes.hpp"

#include "libfive/render/brep/hybrid/hybrid_tree.hpp"

namespace libfive {

template <unsigned N>
class HybridNeighbors :
    public Neighbors<N, HybridTree<N>, HybridNeighbors<N>>
{
public:
    /*  Constructor, returning an empty neighbor array */
    HybridNeighbors()
        : Neighbors<N, HybridTree<N>, HybridNeighbors<N>>()
    {
        // Nothing to do here
    }

    /*
     *  The same subspace may be shared between multiple cells.
     *
     *  This function applies a user-provided function to every
     *  instance of the shared subspace that is reachable with
     *  the current set of neighbors.
     *
     *  It function also walks down branching neighbors, e.g. to
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
    template <typename F>
    void map(NeighborIndex i, F& f) const {
        // Apply the function to every direct neighbor
        for (const auto& t : NeighborTables<N>::neighborTable(i)) {
            const auto n = this->neighbors[t.first.i];
            if (n != nullptr && n->leaf != nullptr) {
                f(n, t.second);
            }
        }

        // Then search down the tree, if this is a corner
        if (i.isCorner()) {
            for (const auto& t : NeighborTables<N>::cornerTable(i.pos())) {
                auto n = this->neighbors[t.first.i];
                if (n != nullptr && n->isBranch()) {
                    while (n->isBranch()) {
                        n = n->children[t.second.i].load();
                        assert(n != nullptr);
                    }
                    if (n->leaf != nullptr) {
                        f(n, t.second.neighbor());
                    }
                }
            }
        }
    }
};

}   // namespace libfive


