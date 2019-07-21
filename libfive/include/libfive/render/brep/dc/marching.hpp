/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <array>
#include <memory>
#include <set>

#include "libfive/render/brep/edge_tables.hpp"

namespace libfive {

template <unsigned N>
class MarchingTable
{
public:
    /*  Represents an edge as a corner-to-corner mapping */
    typedef std::pair<int, int> Edge;

    /*  Represents the set of edges that define a particular patch
     *  There may not be _edges(N) edges for a particular patch;
     *  use -1 to terminate the array  */
    using PatchEdges = std::array<Edge, _edges(N)>;

    /*  Represents a full set of patches
     *  Use an empty patch (-1) to terminate */
    using Patches = std::array<PatchEdges, ipow(2, N - 1)>;

    /*  Represents a full Marching cubes or squares table  */
    using VertsToPatches = std::array<Patches, ipow(2, _verts(N))>;

    /* VertsToEdge is indexed by [a][b] where a and b are vertices, and
     * returns an edge index (0 to 2*_edges(N) - 1)
     * The edge index is only used for indexing EdgeToPatch */
    using VertsToEdge = std::array<std::array<int, _verts(N)>, _verts(N)>;

    /* EdgeToPatch is indexed by [mask][edge] where mask is a corner bitmask and
     * edge is from VertsToEdge.  It returns a patch index (0-3 for 3D case,
     * 0-1 for 2D case) */
    using EdgeToPatch = std::array<std::array<int, _edges(N) * 2>,
                                   ipow(2, _verts(N))>;


    static const Patches& v(CornerIndex i);
    static const std::array<int, _verts(N)>& e(CornerIndex i);
    static const std::array<int, _edges(N) * 2>& p(CornerIndex i);

protected:
    static const MarchingTable& instance();
    MarchingTable();

    VertsToPatches v_data;
    VertsToEdge e_data;
    EdgeToPatch p_data;
};

}   // namespace libfive
