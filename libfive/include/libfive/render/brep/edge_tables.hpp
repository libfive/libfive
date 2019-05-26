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

namespace Kernel {

/*  Returns the number of vertices in an N-dimensional cube */
static constexpr int _verts(unsigned N)
{ return ipow(2, N); }

/*  Returns the number of edges in an N-dimensional cube */
static constexpr int _edges(unsigned N)
{ return (N == 0) ? 0 : (_edges(N - 1) * 2 + _verts(N - 1)); }

template <unsigned N>
struct EdgeTables
{
    /*  For each subspace in an N-dimensional subspace cell, returns the
     *  subspaces which it is connected to */
    static std::array<boost::container::static_vector<
            NeighborIndex, ipow(3, N) - 1>,
        ipow(3, N)> neighbors;

    /*  Used as a flag to trigger population of the static arrays */
    static bool buildTables();
    static bool loaded;
};

//  We explicitly instantiate the EdgeTables classes in edge_tables.cpp
extern template struct EdgeTables<2>;
extern template struct EdgeTables<3>;

}   // namespace Kernel
