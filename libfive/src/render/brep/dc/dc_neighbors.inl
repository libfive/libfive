/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/dc/dc_neighbors.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Interval::State DCNeighbors<N>::check(CornerIndex corner) const
{
    for (const auto& t : NeighborTables<N>::cornerTable(corner))
    {
        if (this->neighbors[t.first.i] != nullptr) {
            return this->neighbors[t.first.i]->cornerState(t.second.i);
        }
    }

    return Interval::UNKNOWN;
}

template <unsigned N>
std::pair<const DCTree<N>*, unsigned> DCNeighbors<N>::checkConsistency(
        CornerIndex corner,
        const Interval::State s) const
{
    for (const auto& t : NeighborTables<N>::cornerTable(corner))
    {
        auto n = this->neighbors[t.first.i];
        if (this->neighbors[t.first.i] != nullptr) {
            while (n->isBranch()) {
                n = n->children[t.second.i].load();
            }
            if (n->cornerState(t.second.i) != s) {
                return std::make_pair(n, t.second.i);
            }
        }
    }
    return std::make_pair(nullptr, 0);
}


template <unsigned N>
Intersection<N>* DCNeighbors<N>::check(uint8_t a, uint8_t b) const
{
    // This is actually quite beautiful:
    //
    // We walk through the possible corner tables for each corner,
    // looking for a match where both potential corners are on the
    // same neighbor and that neighbor is present.
    //
    // Because the cornerTable arrays are sorted based on neighbor,
    // we can do this in a single pass through the two tables,
    // using their iterators.
    //
    // This is admittedly code that took maximum cleverness to write,
    // so think carefully about debugging it...
    auto itr_a = NeighborTables<N>::cornerTable(a).begin();
    auto itr_b = NeighborTables<N>::cornerTable(b).begin();

    while (itr_a != NeighborTables<N>::cornerTable(a).end() &&
           itr_b != NeighborTables<N>::cornerTable(b).end())
    {
        if (itr_a->first.i < itr_b->first.i) {
            itr_a++;
        } else if (itr_b->first.i < itr_a->first.i) {
            itr_b++;
        }
        else if (this->neighbors[itr_a->first.i] != nullptr) {
            return this->neighbors[itr_a->first.i]->intersection(
                    itr_a->second.i, itr_b->second.i);
        } else {
            itr_a++;
            itr_b++;
        }
    }
    return nullptr;
}

}   // namespace libfive
