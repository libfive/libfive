/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <bitset>

#include "libfive/render/brep/edge_tables.hpp"

namespace Kernel {

template <unsigned N>
std::array<boost::container::static_vector<NeighborIndex, ipow(3, N) - 1>,
           ipow(3, N)> EdgeTables<N>::neighbors;

template <unsigned N>
bool EdgeTables<N>::loaded = EdgeTables<N>::buildTables();

template <unsigned N>
bool EdgeTables<N>::buildTables()
{
    for (unsigned i=0; i < ipow(3, N); ++i) {
        for (unsigned j=0; j < ipow(3, N); ++j) {
            NeighborIndex a(i);
            NeighborIndex b(j);
            if (i != j && a.contains(b)) {
                neighbors[i].push_back(b);
            }
        }
    }

    return true;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of template
template struct EdgeTables<2>;
template struct EdgeTables<3>;

}   // namespace Kernel
