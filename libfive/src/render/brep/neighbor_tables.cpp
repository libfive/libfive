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

template <unsigned N>
std::array<
    std::array<std::pair<NeighborIndex, CornerIndex>, ipow(2, N) - 1>,
    ipow(2, N)> NeighborTables<N>::cornerTable;

template <unsigned N>
std::array<
    boost::container::static_vector<std::pair<NeighborIndex, NeighborIndex>,
                                    ipow(2, N)>,
    ipow(3, N)> NeighborTables<N>::neighborTable;

template <unsigned N>
std::array<
    boost::container::static_vector<std::pair<CornerIndex, NeighborIndex>,
                                    ipow(3, N)>,
    ipow(3, N)> NeighborTables<N>::qefSumTable;

template <unsigned N> bool NeighborTables<N>::loaded =
    NeighborTables<N>::buildTables();

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
bool NeighborTables<N>::buildTables()
{
    for (unsigned c=0; c < ipow(2, N); ++c) {
        for (unsigned n=0; n < ipow(3, N) - 1; ++n) {
            pushIndexTable[c][n] = pushIndex(CornerIndex(c), NeighborIndex(n));
        }
    }

    for (unsigned c=0; c < ipow(2, N); ++c) {
        unsigned i = 0;
        for (unsigned n = 0; n < ipow(3, N) - 1; ++n) {
            auto p = getCorner(c, n);
            if (p.i != -1) {
                cornerTable[c][i++] = std::make_pair(NeighborIndex(n), p);
            }
        }
        assert(i == ipow(2, N) - 1);
    }

    for (unsigned s=0; s < ipow(3, N); ++s) {
        for (unsigned n = 0; n < ipow(3, N) - 1; ++n) {
            auto p = getNeighbor(s, n);
            if (p.i != -1) {
                neighborTable[s].push_back(std::make_pair(NeighborIndex(n), p));
            }
        }
        assert(neighborTable[s].size() ==
               ipow(2, N - NeighborIndex(s).dimension()) - 1);
    }

    // Iterate over every child, finding which QEFs should be
    // collected and summed into larger QEFs.
    //
    // To avoid double-counting, we skip the low subspaces on high children,
    // e.g. the cell marked with an X adds every QEF marked with a *
    //
    //    -------------        -------------
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    *--*--*------        ---------*--*
    //    |     |     |        |     |     |
    //    *  X  *     |        |     |  X  *
    //    |     |     |        |     |     |
    //    *--*--*------        ---------*--*
    //
    //    ---------*--*        *--*--*------
    //    |     |     |        |     |     |
    //    |     |  X  *        *  X  *     |
    //    |     |     |        |     |     |
    //    -------------        -------------
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    |     |     |        |     |     |
    //    -------------        -------------
    for (unsigned i=0; i < ipow(2, N); ++i) {
        for (unsigned j=0; j < ipow(3, N); ++j) {
            const auto child = CornerIndex(i);
            const auto neighbor = NeighborIndex(j);
            const auto fixed = neighbor.fixed<N>();
            const auto floating = neighbor.floating();
            const auto pos = neighbor.pos();

            // For every fixed axis, it must either be high,
            // or the child position on said axis must be low
            bool valid = true;
            for (unsigned d=0; d < N; ++d) {
                if (fixed & (1 << d)) {
                    valid &= (pos & (1 << d)) || (!(child.i & (1 << d)));
                }
            }

            if (!valid) {
                continue;
            }

            // Next, we need to figure out how to map the child's subspace
            // into the parent subspace.
            //
            // Every floating axis remains floating
            // Each fixed axis remains fixed if it agrees with the corner axis,
            // otherwise it's converted to a floating axis.
            uint8_t floating_out = 0;
            uint8_t pos_out = 0;

            for (unsigned d=0; d < N; ++d) {
                if (floating & (1 << d) ||
                   (pos & (1 << d)) != (child.i & (1 << d)))
                {
                    floating_out |= (1 << d);
                }
                else
                {
                    pos_out |= pos & (1 << d);
                }
            }
            const auto target = NeighborIndex::fromPosAndFloating(
                    pos_out, floating_out);

            qefSumTable[target.i].push_back(std::make_pair(i, j));
        }
    }

    return true;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of template
template struct NeighborTables<2>;
template struct NeighborTables<3>;

}   // namespace Kernel
