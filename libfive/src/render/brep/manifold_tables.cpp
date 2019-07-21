#include <iostream>
/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <set>

#include "libfive/render/brep/edge_tables.hpp"
#include "libfive/render/brep/manifold_tables.hpp"
#include "libfive/render/brep/util.hpp"

namespace libfive {

template <unsigned N>
bool checkBitfield(uint32_t b) {
    uint32_t new_inside = 0;
    for (unsigned i=0; i < ipow(3, N) && !new_inside; ++i) {
        if (b & (1 << i)) {
            new_inside = (1 << i);
        }
    }

    //  A completely-empty or completely-filled region isn't a topological disk
    if (new_inside == 0 || new_inside == (1 << ipow(3, N)) - 1) {
        return false;
    }

    // We'll flood-fill from a starting point for each set.
    uint32_t connected_inside = 0;

    while (new_inside) {
        uint32_t next_new_inside = 0;
        for (unsigned i=0; i < ipow(3, N); ++i) {
            const auto mask = (1 << i);

            // Accumulate new inside
            if ((new_inside & mask) && !(connected_inside & mask)) {
                connected_inside |= mask;
                for (auto& n : EdgeTables<N>::boundary(i)) {
                    const uint32_t neighbor_mask = (1 << n.i);
                    if (b & neighbor_mask) {
                        next_new_inside |= neighbor_mask;
                    }
                }
            }
        }
        new_inside = next_new_inside;
    }
    return connected_inside == b;
}

template <unsigned N>
ManifoldTables<N>::ManifoldTables()
{
    /*  For dimension N, we calculate whether the bitfield of inside / outside
     *  boundary subspaces is manifold or not.  Here, "mainfold" means
     *  "topologically equivalent to a disk", i.e. all of the inside points
     *  are connected, and all of the outside points are connected.
     *
     *  For example, in 2D
     *
     *  Manifold:       Not manifold:
     *  X--o--o         X--o--o
     *  |     |         |     |
     *  X     o         X     X
     *  |     |         |     |
     *  X--X--o         X--o--o
     *
     *  This is used when deciding how to place a vertex in the center
     *  of the subspace.  If the boundary of the subspace is non-manifold,
     *  then we shouldn't use Dual Conturing to place the vertex.
     */

    // We skip the center, and subtract one bit because we'll assume the top
    // bit is zero (to cut the space in half)
    constexpr auto bits = ipow(3, N) - 2;
    data.resize(1 << bits, 0);
    known.resize(1 << bits, 0);
}

template <unsigned N>
bool ManifoldTables<N>::manifold(uint32_t b)
{
    static ManifoldTables<N> singleton;
    constexpr uint32_t top_bit = 1 << (ipow(3, N) - 2);

    // We check both the positive and negative of this bitfield,
    // because we're only flood-filling one sign.
    const uint32_t inv = (~b) & ((top_bit << 1) - 1);

    // Manifoldness is symmetric to flipping the sign, so we only store
    // half of the table (with the top bit always set to zero).
    const uint32_t index = (b & top_bit) ? inv : b;

    // Lazily populate the full data array
    if (!singleton.known.at(index)) {
        singleton.data.at(index) = checkBitfield<N>(b) && checkBitfield<N>(inv);
    }
    return singleton.data.at(index);
}

// Explicit initialization of template
template class ManifoldTables<2>;
template class ManifoldTables<3>;

}
