/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "../neighbors.inl"
#include "dc_neighbors.inl"

namespace libfive {

Interval::State PseudoDCNeighbors2::check(CornerIndex corner) const
{
    for (const auto& t : NeighborTables<2>::cornerTable(corner))
    {
        if (this->neighbors[t.first.i] != nullptr) {
            return this->neighbors[t.first.i]->cornerState(t.second.i);
        }
    }

    return Interval::UNKNOWN;
}

std::pair<const PseudoDCTree2*, unsigned> PseudoDCNeighbors2::checkConsistency(
        CornerIndex corner,
        const Interval::State s) const
{
    for (const auto& t : NeighborTables<2>::cornerTable(corner))
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


Intersection<2>* PseudoDCNeighbors2::check(uint8_t a, uint8_t b) const
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
    auto itr_a = NeighborTables<2>::cornerTable(a).begin();
    auto itr_b = NeighborTables<2>::cornerTable(b).begin();

    while (itr_a != NeighborTables<2>::cornerTable(a).end() &&
           itr_b != NeighborTables<2>::cornerTable(b).end())
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

template class Neighbors<2, DCTree<2>, DCNeighbors<2>>;
template class Neighbors<2, PseudoDCTree2, PseudoDCNeighbors2>;
template class DCNeighbors<2>;
}   // namespace libfive
