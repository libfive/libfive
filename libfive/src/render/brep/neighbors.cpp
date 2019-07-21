/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"

namespace libfive {

template <unsigned N, typename T, typename C>
Neighbors<N, T, C>::Neighbors() {
    std::fill(neighbors.begin(), neighbors.end(), nullptr);
}

template <unsigned N, typename T, typename C>
C Neighbors<N, T, C>::push(uint8_t child,
           const std::array<std::atomic<T*>, 1 << N>& children) const
{
    C out;
    for (unsigned i=0; i < ipow(3, N) - 1; ++i)
    {
        const auto q = NeighborTables<N>::pushIndexTable(child)[i];
        if (q.first.i == -1) {
            out.neighbors[i] = children[q.second.i].load();
        } else if (neighbors[q.first.i]) {
            out.neighbors[i] = neighbors[q.first.i]->
                children[q.second.i].load();
        }
    }

    return out;
}

}   // namespace libfive

