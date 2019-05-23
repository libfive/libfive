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
std::array<std::pair<CornerIndex, CornerIndex>, _edges(N)> EdgeTables<N>::t;

template <unsigned N>
bool EdgeTables<N>::loaded = EdgeTables<N>::buildTables();

template <unsigned N>
bool EdgeTables<N>::buildTables()
{
    unsigned n = 0;
    for (unsigned i=0; i < ipow(2, N); ++i) {
        for (unsigned j=0; j < i; ++j) {
            if (std::bitset<8>(i ^ j).count() == 1) {
                t[n++] = std::make_pair(CornerIndex(i), CornerIndex(j));
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
