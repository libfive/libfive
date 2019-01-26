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
            const auto index = n->leaf->sub[t.second.i].load()->index.load();
            if (index != 0) {
                return index;
            }
        }
    }

    // If this is a corner, it could have a defined index deeper down
    // one of the neighbors.  For example, if we're in cell X and looking
    // for corner C, our neighbor isn't a leaf, but we can travel down it's
    // branching structure to find the corner index
    //
    //   ---------
    //   |   |   |
    //   ---------
    //   |-|-|-|-|
    //   ====C====
    //   | X |   |
    //   ---------
    //   |   |   |
    //   ---------
    //
    if (i.isCorner()) {
        for (const auto& t : NeighborTables<N>::cornerTable[i.pos()]) {
            auto n = this->neighbors[t.first.i];
            if (n != nullptr) {
                while (n->isBranch()) {
                    n = n->children[t.second.i].load();
                    assert(n != nullptr);
                }
                assert(n->leaf != nullptr);
                const auto index = n->leaf->sub[t.second.neighbor().i].load()
                                    ->index.load();
                if (index != 0) {
                    return index;
                }
            }
        }
    }

    return 0;
}

template <unsigned N>
std::pair<uint64_t, bool> SimplexNeighbors<N>::getIndexAndBranching(
        NeighborIndex i) const
{
    uint64_t out = 0;
    bool has_branching_neighbor = false;
    for (const auto& t : NeighborTables<N>::neighborTable[i.i]) {
        const auto n = this->neighbors[t.first.i];
        if (n != nullptr) {
            if (n->isBranch()) {
                has_branching_neighbor = true;
            } else {
                assert(n->leaf != nullptr);
                auto index = n->leaf->sub[t.second.i].load()->index.load();
                if (index != 0) {
                    assert(out == 0 || out == index);
                    out = index;
                }
            }
        }
    }

    return std::make_pair(out, has_branching_neighbor);
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
