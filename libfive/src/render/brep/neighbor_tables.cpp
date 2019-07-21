/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/neighbor_tables.hpp"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////
// Static variables
template <unsigned N>
NeighborTables<N>::NeighborTables()
{
    for (unsigned c=0; c < ipow(2, N); ++c) {
        for (unsigned n=0; n < ipow(3, N) - 1; ++n) {
            pushIndexTable_data[c][n] = pushIndex(CornerIndex(c),
                                                  NeighborIndex(n));
        }
    }

    for (unsigned c=0; c < ipow(2, N); ++c) {
        unsigned i = 0;
        for (unsigned n = 0; n < ipow(3, N) - 1; ++n) {
            auto p = getCorner(c, n);
            if (p.i != -1) {
                cornerTable_data[c][i++] = std::make_pair(NeighborIndex(n), p);
            }
        }
        assert(i == ipow(2, N) - 1);
    }

    for (unsigned s=0; s < ipow(3, N); ++s) {
        for (unsigned n = 0; n < ipow(3, N) - 1; ++n) {
            auto p = getNeighbor(s, n);
            if (p.i != -1) {
                neighborTable_data[s].push_back(std::make_pair(NeighborIndex(n), p));
            }
        }
        assert(neighborTable_data[s].size() ==
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

            qefSumTable_data[target.i].push_back(std::make_pair(i, j));
        }
    }
}

template <unsigned N>
const typename NeighborTables<N>::PushIndexArray&
NeighborTables<N>::pushIndexTable(CornerIndex c)
{
    return instance().pushIndexTable_data[c.i];
}

template <unsigned N>
const typename NeighborTables<N>::CornerTableArray&
NeighborTables<N>::cornerTable(CornerIndex c)
{
    return instance().cornerTable_data[c.i];
}

template <unsigned N>
const typename NeighborTables<N>::NeighborTableVec&
NeighborTables<N>::neighborTable(NeighborIndex n)
{
    return instance().neighborTable_data[n.i];
}

template <unsigned N>
const typename NeighborTables<N>::QEFTableVec&
NeighborTables<N>::qefSumTable(NeighborIndex n)
{
    return instance().qefSumTable_data[n.i];
}

template <unsigned N>
const NeighborTables<N>& NeighborTables<N>::instance()
{
    static NeighborTables<N> singleton;
    return singleton;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of template
template class NeighborTables<2>;
template class NeighborTables<3>;

}   // namespace libfive
