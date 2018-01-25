/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once
#include <array>
#include <memory>
#include <set>

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////

/*  Compile-time power */
static constexpr int _pow(unsigned P, unsigned N)
{ return (N == 0) ? 1 : P * _pow(P, N - 1); }

static constexpr unsigned _upow(unsigned P, unsigned N)
{ return (N == 0) ? 1 : P * _upow(P, N - 1); }

/*  Returns the number of vertices in an N-dimensional cube */
static constexpr int _verts(unsigned N)
{ return _pow(2, N); }

/*  Returns the number of edges in an N-dimensional cube */
static constexpr int _edges(unsigned N)
{ return (N == 0) ? 0 : (_edges(N - 1) * 2 + _verts(N - 1)); }

////////////////////////////////////////////////////////////////////////////////

namespace Marching
{

/*  Represents an edge as a corner-to-corner mapping */
typedef std::pair<int, int> Edge;

/*  Represents the set of edges that define a particular patch
 *  There may not be _edges(N) edges for a particular patch;
 *  use -1 to terminate the array  */
template <unsigned N>
using PatchEdges = std::array<Edge, _edges(N)>;

/*  Represents a full set of patches
 *  Use an empty patch (-1) to terminate */
template <unsigned N>
using Patches = std::array<PatchEdges<N>, _pow(2, N - 1)>;

/*  Represents a full Marching cubes or squares table  */
template <unsigned N>
using VertsToPatches = std::array<Patches<N>, _pow(2, _verts(N))>;

// VertsToEdge is indexed by [a][b] where a and b are vertices, and
// returns an edge index (0 to 2*_edges(N) - 1)
// The edge index is only used for indexing EdgeToPatch
template <unsigned N>
using VertsToEdge = std::array<std::array<int, _verts(N)>, _verts(N)>;

// EdgeToPatch is indexed by [mask][edge] where mask is a corner bitmask and
// edge is from VertsToEdge.  It returns a patch index (0-3 for 3D case,
// 0-1 for 2D case)
template <unsigned N>
using EdgeToPatch = std::array<std::array<int, _edges(N) * 2>,
                               _pow(2, _verts(N))>;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
struct MarchingTable
{
    VertsToPatches<N> v;
    VertsToEdge<N> e;
    EdgeToPatch<N> p;
};

template <unsigned N>
std::unique_ptr<MarchingTable<N>> buildTable();

}   // namespace Marching
}   // namespace Kernel
