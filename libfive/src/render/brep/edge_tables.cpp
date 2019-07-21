/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/edge_tables.hpp"

namespace libfive {

template <unsigned N>
EdgeTables<N>::EdgeTables()
{
    for (unsigned i=0; i < ipow(3, N); ++i) {
        for (unsigned j=0; j < ipow(3, N); ++j) {
            NeighborIndex a(i);
            NeighborIndex b(j);
            if (i != j && (a.contains(b) || b.contains(a))) {
                neighbor_table[i].push_back(b);
                if (b.dimension() < N) {
                    boundary_table[i].push_back(b);
                }
                if (a.contains(b)) {
                    subspace_table[i].push_back(b);
                }
            }
        }
    }
}

template <unsigned N>
const typename EdgeTables<N>::Table& EdgeTables<N>::neighbors(NeighborIndex n)
{
    return instance().neighbor_table[n.i];
}

template <unsigned N>
const typename EdgeTables<N>::Table& EdgeTables<N>::subspaces(NeighborIndex n)
{
    return instance().subspace_table[n.i];
}

template <unsigned N>
const typename EdgeTables<N>::Table& EdgeTables<N>::boundary(NeighborIndex n)
{
    return instance().boundary_table[n.i];
}

template <unsigned N>
const EdgeTables<N>& EdgeTables<N>::instance()
{
    static EdgeTables<N> singleton;
    return singleton;
}

// Explicit initialization of template
template class EdgeTables<2>;
template class EdgeTables<3>;

}   // namespace libfive
