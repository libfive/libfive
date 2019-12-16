/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <boost/container/static_vector.hpp>

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/indexes.hpp"

namespace libfive {

/*  Returns the number of vertices in an N-dimensional cube */
static constexpr int _verts(unsigned N)
{ return ipow(2, N); }

/*  Returns the number of edges in an N-dimensional cube */
static constexpr int _edges(unsigned N)
{ return (N == 0) ? 0 : (_edges(N - 1) * 2 + _verts(N - 1)); }

template <unsigned N>
class EdgeTables
{
public:
    using Table = boost::container::static_vector<
        NeighborIndex, ipow(3, N) - 1>;

    /*  For each subspace in an N-dimensional subspace cell, returns
     *  the lower-dimensional subspaces which it is connected to.
     *
     *  For example, in 2D,
     *  A--B--C
     *  | \|/ |
     *  D--E--F
     *  | /|\ |
     *  G--H--I
     *
     *  B connects to A and C, A connects to nothing, D connects
     *  to A and G, E, connects to everything, etc.
     */
    static const Table& subspaces(NeighborIndex n);

    /*  For each subspace in an N-dimensional cell, returns all of the
     *  subspaces which it is connected to (of higher and lower dimension). */
    static const Table& neighbors(NeighborIndex n);

    /*  For each subspace in an N-dimensional cell, returns all of the
     *  subspaces which it is connected to (of dimensions lower than N).
     *
     *  In the example given above, A is connected to B and D, but not E.
     *
     *  This represents connectivity along the N-1 dimensional boundary
     *  of an N-dimensional cell. */
    static const Table& boundary(NeighborIndex n);

protected:
    static const EdgeTables& instance();

    EdgeTables();
    std::array<Table, ipow(3, N)> subspace_table;
    std::array<Table, ipow(3, N)> neighbor_table;
    std::array<Table, ipow(3, N)> boundary_table;
};

extern template class EdgeTables<2u>;
extern template class EdgeTables<3u>;

}   // namespace libfive
