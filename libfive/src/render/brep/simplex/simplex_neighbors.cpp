/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/simplex/simplex_neighbors.hpp"
#include "libfive/render/brep/simplex/simplex_tree.hpp"

namespace Kernel {

template <unsigned N>
SimplexNeighbors<N>::SimplexNeighbors()
    : Neighbors<N, SimplexTree<N>, SimplexNeighbors<N>>()
{
    // Nothing to do here
}

template <unsigned N>
uint64_t SimplexNeighbors<N>::getIndex(NeighborIndex i) const
{
    for (const auto& t : NeighborTables<N>::neighborTable[i.i]) {
        const auto n = this->neighbors[t.first.i];
        if (n != nullptr && n->leaf != nullptr) {
            auto index = n->leaf->sub[t.second.i]->index;
            if (index != 0) {
                return index;
            }
        }
    }

    /*  Special-casing for this particular situation:
     *
     *  -----------------------------------------
     *  |                    |         |        |
     *  |                    |         |        |
     *  |                    |         |        |
     *  |                    ----------X---------
     *  |                    |         |        |
     *  |                    |    i    |        |
     *  |                    |         |        |
     *  ---------------------C-------------------
     *  |                    |                  |
     *  |                    |                  |
     *  |         N          |                  |
     *  |                    |                  |
     *  |                    |                  |
     *  |                    |                  |
     *  -----------------------------------------
     *
     *  If we're the cell labelled N, our upper-right neighbor
     *  is the cell marked with an X.  If we want to find the
     *  corner marked C, we need to recurse down into X, to cell i.
     */
    if (i.isCorner()) {
        for (auto& c: NeighborTables<N>::cornerTable[i.pos()]) {
            if (this->neighbors[c.first.i] != nullptr) {
                auto n = this->neighbors[c.first.i];
                while (n->isBranch()) {
                    assert(n->leaf == nullptr);
                    n = n->children[c.second.i].load();
                    assert(n != nullptr);
                }
                assert(n->leaf != nullptr);
                auto index = n->leaf->sub[c.second.neighbor().i]->index;
                if (index != 0) {
                    return index;
                }
            }
        }
    }
    return 0;
}

template <unsigned N>
std::pair<const SimplexLeaf<N>*, NeighborIndex> SimplexNeighbors<N>::check(NeighborIndex i) const
{
    for (const auto& t : NeighborTables<N>::neighborTable[i.i]) {
        const auto n = this->neighbors[t.first.i];
        if (n != nullptr && n->leaf != nullptr) {
            return std::make_pair(n->leaf, t.second.i);
        }
    }
    return std::make_pair(nullptr, 0);
}

}   // namespace Kernel
