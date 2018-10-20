/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/dc/neighbors.hpp"
#include "libfive/render/brep/dc/xtree.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"

namespace Kernel {

template <unsigned N>
Neighbors<N>::Neighbors() {
    std::fill(neighbors.begin(), neighbors.end(), nullptr);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Neighbors<N> Neighbors<N>::push(uint8_t child,
        const std::array<std::atomic<XTree<N>*>, 1 << N>&
            children)
{
    Neighbors out;
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
    return out;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Interval::State Neighbors<N>::check(uint8_t corner) const
{
    for (const auto& t : NeighborTables<N>::cornerTable[corner])
    {
        if (neighbors[t.first.i] != nullptr) {
            return neighbors[t.first.i]->cornerState(t.second.i);
        }
    }

    return Interval::UNKNOWN;
}

template <unsigned N>
std::shared_ptr<IntersectionVec<N>> Neighbors<N>::check(
        uint8_t a, uint8_t b) const
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
    auto itr_a = NeighborTables<N>::cornerTable[a].begin();
    auto itr_b = NeighborTables<N>::cornerTable[b].begin();

    while (itr_a != NeighborTables<N>::cornerTable[a].end() &&
           itr_b != NeighborTables<N>::cornerTable[b].end())
    {
        if (itr_a->first.i < itr_b->first.i) {
            itr_a++;
        } else if (itr_b->first.i < itr_a->first.i) {
            itr_b++;
        }
        else if (neighbors[itr_a->first.i] != nullptr) {
            return neighbors[itr_a->first.i]->intersection(
                    itr_a->second.i, itr_b->second.i);
        } else {
            itr_a++;
            itr_b++;
        }
    }
    return nullptr;
}

// Explicit initialization of template
template class Neighbors<2>;
template class Neighbors<3>;

}   // namespace Kernel
