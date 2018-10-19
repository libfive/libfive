/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>
#include <fstream>

#include "libfive/render/brep/simplex/simplex_mesher.hpp"
#include "libfive/render/brep/simplex/simplextree.hpp"
#include "libfive/render/brep/simplex/solver.hpp"

namespace Kernel {

struct FacePair {
    unsigned a;
    unsigned b;
    Axis::Axis axis;
};

template <Axis::Axis A>
void SimplexMesher::load(const std::array<const SimplexTree<3>*, 4>& ts)
{
    // For each cell, we need to generate 4 tetrahedrons, which are
    //  edge, corner+, face A, center
    //  edge, corner+, face B, center
    //  edge, corner-, face A, center
    //  edge, corner-, face B, center
    //
    // The vertices at edge, corner+, and corner- are shared between all 4 cells
    // Face vertices are shared by two cells.
    // Center vertices are unique to each cell.
    //
    // In all cases, if cells are different sizes, then we must select the
    // minimum-level cell to pull the simplex vertex from.

    const auto index = std::min_element(ts.begin(), ts.end(),
            [](const SimplexTree<3>* a, const SimplexTree<3>* b)
            { return a->leafLevel() < b->leafLevel(); }) - ts.begin();

    // All cells are EMPTY or FILLED, so we return early
    if (ts[index]->leafLevel() == UINT32_MAX) {
        return;
    }

    assert(ts[index]->leaf != nullptr);

    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    const unsigned corner_index_a = index ^ (Q | R);
    const unsigned corner_index_b = corner_index_a ^ A;

    const unsigned corner_simplex_a = SimplexSolver::cornerToSimplex(corner_index_a);
    const unsigned corner_simplex_b = SimplexSolver::cornerToSimplex(corner_index_b);
    const unsigned edge_simplex = SimplexSolver::simplexUnion(
            corner_simplex_a, corner_simplex_b);

    const Eigen::Vector3f vert_corner_a = ts[index]->leaf->verts.row(corner_simplex_a);
    const Eigen::Vector3f vert_corner_b = ts[index]->leaf->verts.row(corner_simplex_b);
    const Eigen::Vector3f vert_edge = ts[index]->leaf->verts.row(edge_simplex);

    std::array<FacePair, 4> face_pairs =
        {{{0, 1, Q}, {0, 2, R}, {2, 3, Q}, {1, 3, R}}};

    Eigen::Matrix<double, 4, 3> face_simplexes;

    for (unsigned i=0; i < 4; ++i)
    {
        // Pull trees from the array
        auto ta = ts[face_pairs[i].a];
        auto tb = ts[face_pairs[i].b];

        // Index of smaller tree
        unsigned ti = 0;

        if (ta->leafLevel() == UINT32_MAX && tb->leafLevel() == UINT32_MAX) {
            continue;
        } else if (ta->leafLevel() < tb->leafLevel()) {
            ti = face_pairs[i].a;
        } else {
            ti = face_pairs[i].b;
        }
    }

    constexpr std::array<uint8_t, 4> corners = {{Q|R, R, Q, 0}};

    // If there is a sign change across the relevant edge, then call the
    // watcher with the segment corners (with proper winding order)
    auto a = ts[index]->cornerState(corners[index]);
    auto b = ts[index]->cornerState(corners[index] | A);
    if (a != b)
    {
        if (a != Interval::FILLED)
        {
            load<A, 0>(ts);
        }
        else
        {
            load<A, 1>(ts);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

}   // namespace Kernel
