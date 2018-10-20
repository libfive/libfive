/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/neighbor_tables.hpp"

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////
// Static variables
template <unsigned N>
std::array<
    std::array<std::pair<NeighborIndex, CornerIndex>, ipow(3, N) - 1>,
        ipow(2, N)> NeighborTables<N>::pushIndexTable;

template <unsigned N> bool NeighborTables<N>::loaded =
    NeighborTables<N>::buildTables();

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
bool NeighborTables<N>::buildTables()
{
    for (unsigned c=0; c < ipow(2, N); ++c) {
        for (unsigned n=0; n < ipow(3, N); ++n) {
            pushIndexTable[c][n] = pushIndex(CornerIndex(c), NeighborIndex(n));
        }
    }
    return true;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of template
template struct NeighborTables<2>;
template struct NeighborTables<3>;

}   // namespace Kernel
