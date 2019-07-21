/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/simplex/simplex_neighbors.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"
#include "libfive/render/brep/simplex/simplex_tree.hpp"

namespace libfive {

template <unsigned N>
SimplexNeighbors<N>::SimplexNeighbors()
    : Neighbors<N, SimplexTree<N>, SimplexNeighbors<N>>()
{
    // Nothing to do here
}

template <unsigned N>
SimplexLeafSubspace<N>* SimplexNeighbors<N>::getSubspace(NeighborIndex i) const
{
    SimplexLeafSubspace<N>* out = nullptr;
    for (const auto& t : NeighborTables<N>::neighborTable(i)) {
        const auto n = this->neighbors[t.first.i];
        if (n != nullptr && n->leaf != nullptr) {
            auto ptr = n->leaf->sub[t.second.i].load();
            if (reinterpret_cast<uintptr_t>(ptr) >
                reinterpret_cast<uintptr_t>(out))
            {
                out = ptr;
            }
        }
    }

    // If this is a corner, it could have a subspace deeper down
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
        for (const auto& t : NeighborTables<N>::cornerTable(i.pos())) {
            auto n = this->neighbors[t.first.i];
            if (n != nullptr) {
                while (n->isBranch()) {
                    n = n->children[t.second.i].load();
                    assert(n != nullptr);
                }
                assert(n->leaf != nullptr);
                auto ptr = n->leaf->sub[t.second.neighbor().i].load();
                if (reinterpret_cast<uintptr_t>(ptr) >
                    reinterpret_cast<uintptr_t>(out))
                {
                    out = ptr;
                }
            }
        }
    }

    return out;
}

template <unsigned N>
std::pair<const SimplexLeaf<N>*, NeighborIndex> SimplexNeighbors<N>::check(NeighborIndex i) const
{
    for (const auto& t : NeighborTables<N>::neighborTable(i)) {
        const auto n = this->neighbors[t.first.i];
        if (n != nullptr && n->leaf != nullptr) {
            return std::make_pair(n->leaf, t.second.i);
        }
    }
    return std::make_pair(nullptr, 0);
}

}   // namespace libfive
