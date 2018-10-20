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
std::pair<int, int> Neighbors<N>::edgeCheckIndex(
    std::pair<int, int> edge, uint8_t neighbor)
{
    const auto a = NeighborTables<N>::getCorner(edge.first, neighbor);
    const auto b = NeighborTables<N>::getCorner(edge.second, neighbor);

    if (a.i == -1 || b.i == -1)
    {
        return {-1, -1};
    }
    else
    {
        return {a.i, b.i};
    }
}

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
    for (unsigned i=0; i < ipow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = NeighborTables<N>::getCorner(corner, i);
            if (index.i != -1)
            {
                return neighbors[i]->cornerState(index.i);
            }
        }
    }
    return Interval::UNKNOWN;
}

template <unsigned N>
std::shared_ptr<IntersectionVec<N>> Neighbors<N>::check(
        uint8_t a, uint8_t b) const
{
    for (unsigned i=0; i < ipow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = edgeCheckIndex({a, b}, i);
            if (index.first != -1)
            {
                return neighbors[i]->intersection(index.first, index.second);
            }
        }
    }
    return nullptr;
}

// Explicit initialization of template
template class Neighbors<2>;
template class Neighbors<3>;

}   // namespace Kernel
