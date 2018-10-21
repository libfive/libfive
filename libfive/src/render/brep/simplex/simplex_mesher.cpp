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
#include "libfive/render/brep/types.hpp"

namespace Kernel {

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

    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    /*  We'll be storing vertices in this array, in the following order:
     *  0:  edge
     *  1, 2: corners
     *  3: Face between ts[0] and ts[1]
     *  4: Face between ts[0] and ts[2]
     *  5: Face between ts[2] and ts[3]
     *  6: Face between ts[1] and ts[3]
     *  7: ts[0] cell
     *  8: ts[1] cell
     *  9: ts[2] cell
     *  10: ts[3] cell
     */
    struct SubspaceVertex {
        Eigen::Vector3d pos;
        uint64_t index;
        bool inside;
    };
    std::array<SubspaceVertex, 11> subvs;
    auto saveSubspaceVertex = [&subvs, &ts](unsigned index, NeighborIndex s) {
        if (ts.at(index)->leaf) {
            subvs.push_back(SubspaceVertex {
                ts.at(index)->leaf->vertices.row(s.i),
                ts.at(index)->leaf->index.at(s.i),
                ts.at(index)->leaf->inside.at(s.i),
            });
        } else {
            subvs.push_back(SubspaceVertex {
                    Eigen::Vector3d::Zero(),
                    0,
                    false,
            });
        }
    };

    {   /* First, we pick out the vertices on the common edge.  There are
         * three vertices associated with that edge: two on the corners,
         * and one on the edge itself. */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const SimplexTree<3>* a, const SimplexTree<3>* b)
                { return a->leafLevel() < b->leafLevel(); }) - ts.begin();

        // All cells are EMPTY or FILLED, so we return early
        if (ts.at(index)->leafLevel() == UINT32_MAX) {
            return;
        }

        const unsigned corner_index_a = index ^ (Q | R);
        const unsigned corner_index_b = corner_index_a ^ A;
        assert(ts.at(index)->leaf != nullptr);

        const unsigned corner_simplex_a = SimplexSolver::cornerToSimplex(corner_index_a);
        const unsigned corner_simplex_b = SimplexSolver::cornerToSimplex(corner_index_b);
        const unsigned edge_simplex = SimplexSolver::simplexUnion(
                corner_simplex_a, corner_simplex_b);

        saveSubSpaceVertex(index, edge_simplex);
        saveSubSpaceVertex(index, corner_simplex_a);
        saveSubSpaceVertex(index, corner_simplex_b);
    }

    {   // Next, we'll find the vertices shared by faces
        struct FacePair {
            unsigned a;
            unsigned b;
            Axis::Axis axis;
        };
        const std::array<FacePair, 4> face_pairs =
            {{{0, 1, Q}, {0, 2, R}, {2, 3, Q}, {1, 3, R}}};

        for (unsigned i=0; i < 4; ++i)
        {
            // Pull trees from the array
            auto ta = ts.at(face_pairs.at(i).a);
            auto tb = ts.at(face_pairs.at(i).b);

            // Index of smaller tree
            unsigned ti = 0;
            unsigned si = 0;

            if (ta->leafLevel() < tb->leafLevel())
            {
                ti = face_pairs.at(i).a;
                si = SimplexSolver::simplexUnion(
                        SimplexSolver::cornerToSimplex(face_pairs.at(i).axis),
                        SimplexSolver::cornerToSimplex(7));
            }
            else
            {   // This conditional includes the case where both faces are
                // EMPTY or FILLED, which just pushes an empty vertex.
                ti = face_pairs.at(i).b;
                si = SimplexSolver::simplexUnion(
                        SimplexSolver::cornerToSimplex(0),
                        SimplexSolver::cornerToSimplex(((Q | R) ^ face_pairs.at(i).axis) | A));
            }
            saveSubspaceVertex(ti, si);
        }
    }

    // Finally, we'll save the cell vertices
    // There's only one option per cell, since they aren't shared
    for (unsigned i=0; i < 4; ++i)
    {
        saveSubspaceVertex(i, ipow(3, 3) - 1);
    }

    // Now that we've populated our vertex table, we walk around the four cells
    // and perform marching tetrahedrons on 4 tets within each cell.
    //
    // First, we define which six vertices we care about in each cell,
    // in the order
    // 0:   edge
    // 1:   corner
    // 2:   corner
    // 3:   face
    // 4:   face
    // 5:   cell
    //  (with faces ordered clockwise)
    const std::array<std::array<unsigned, 6>, 4> cell_vertices = {{
        {{0, 1, 2, 4, 7, 3}}, // Cell 0
        {{0, 1, 2, 3, 8, 6}}, // Cell 1
        {{0, 1, 2, 5, 9, 4}}, // Cell 2
        {{0, 1, 2, 6, 10, 5}}, // Cell 3
    }};

    //  Within each cell, indexing into the array above, here are the four
    //  tetrahedrons:
    const std::array<std::array<unsigned, 4>, 4> tet_vertices = {{
        {{0, 2, 3, 5}},
        {{0, 2, 1, 5}},
        {{0, 4, 2, 5}},
        {{0, 1, 4, 5}},
    }};

    // This is the marching tetrahedrons table.  For a given bitfield
    // (with bits ordered by cell_vertices), returns a list of edges
    // (as indexed into tet_vertices) that build triangles.
    using EdgePair = std::pair<int, int>;
    using Triangle = std::array<EdgePair, 3>;
    using Patch = std::array<Triangle, 2>;

    /*  Here's one projection of the tetrahedron, with
     *  vertices numbered (0 is farthest from the viewer)
     *         1
     *        /|\
     *       / | \
     *      /  0  \
     *     / /   \ \
     *    2/-------\3
     */
    const std::array<Patch, 16> tet_table = {{
        {{ // 0b0000
        {{ {-1, -1}, {-1, -1}, {-1, -1} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b0001
        {{ { 0,  1}, { 0,  2}, { 0,  3} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b0010
        {{ { 1,  0}, { 1,  3}, { 1,  2} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b0011
        {{ { 1,  2}, { 0,  2}, { 0,  3} }},
        {{ { 0,  3}, { 1,  3}, { 1,  2} }}, }},

        {{ // 0b0100
        {{ { 2,  0}, { 2,  1}, { 2,  3} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b0101
        {{ { 0,  1}, { 2,  1}, { 2,  3} }},
        {{ { 0,  1}, { 2,  3}, { 0,  3} }}, }},

        {{ // 0b0110
        {{ { 1,  3}, { 2,  3}, { 1,  0} }},
        {{ { 1,  0}, { 2,  3}, { 2,  0} }}, }},

        {{ // 0b0111
        {{ { 0,  3}, { 1,  3}, { 2,  3} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b1000
        {{ { 3,  0}, { 3,  2}, { 3,  1} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b1001
        {{ { 3,  2}, { 3,  1}, { 0,  1} }},
        {{ { 0,  1}, { 0,  2}, { 3,  2} }}, }},

        {{ // 0b1010
        {{ { 3,  0}, { 3,  2}, { 1,  0} }},
        {{ { 1,  0}, { 3,  2}, { 1,  2} }}, }},

        {{ // 0b1011
        {{ { 0,  2}, { 3,  2}, { 1,  2} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b1100
        {{ { 2,  0}, { 2,  1}, { 3,  1} }},
        {{ { 3,  0}, { 2,  0}, { 3,  1} }}, }},

        {{ // 0b1110
        {{ { 2,  1}, { 3,  1}, { 0,  1} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b1110
        {{ { 1,  0}, { 3,  0}, { 2,  0} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},

        {{ // 0b1111
        {{ {-1, -1}, {-1, -1}, {-1, -1} }},
        {{ {-1, -1}, {-1, -1}, {-1, -1} }}, }},
    }};

    // Constructor for an ordered pair
    auto Key = [](uint64_t a, uint64_t b) {
        std::make_pair(std::min(a, b), std::max(a, b));
    };
    std::map<std::pair<uint64_t, uint64_t>, uint64_t> edge_search_cache;

    // Iterate over the four cells
    for (unsigned i=0; i < 4; ++i)
    {
        const auto this_cell = ts.at(i);
        // Skip empty or filled cells immediately
        if (this_cell->leaf == nullptr) {
            assert(this_cell->type == Interval::EMPTY ||
                   this_cell->type == Interval::FILLED);
            continue;
        }

        // Iterate over the six tetrahedrons
        const auto& vs = cell_vertices.at(i);
        for (auto& tet: tet_vertices)
        {
            // Build the corner mask
            unsigned mask = 0;
            for (unsigned j=0; j < 4; ++j) {
                mask |= subvs.at(vs.at(tet.at(j)))->inside << j;
            }

            // Iterate over up-to-two triangles
            for (const auto& tri : tet_table.at(mask))
            {
                // If this tet doesn't contain this triangle,
                // then keep going immediately
                if (tri.at(0).first == -1) {
                    break;
                }

                // Iterate over three edges, finding the inside-outside
                // boundary either in a precalculated cache or with binary
                // search.
                Eigen::Matrix<uint32_t, 3, 1> tri_vert_indices;
                for (unsigned t=0; t < tri.size(); ++t)
                {
                    const auto& edge = tri.at(t);
                    const auto& va = subvs.at(cell_vertices.at(tet.at(edge.first)));
                    const auto& vb = subvs.at(cell_vertices.at(tet.at(edge.second)));

                    const auto k = Key(va.index, vb.index);

                    // This edge could be precalculated within the cell (from
                    // a previous cell in this call to load), or within a leaf
                    // (from a different branch of meshing), or both.
                    //
                    // We check for it in both places, populating it into
                    // whichever caches it's missing from.
                    const auto leaf_itr = this_cell->leaf->surface.find(k);
                    const auto cache_itr = edge_search_cache.find(k);
                    if (leaf_itr != this_cell->leaf->surface.end() &&
                        cache_itr != edge_search_cache.end())
                    {
                        assert(*leaf_itr == *cache_itr);
                        tri_vert_indices[t] = *leaf_itr;
                        continue;
                    }
                    else if (leaf_itr != this_cell->leaf->surface.end())
                    {
                        tri_vert_indices[t] = *leaf_itr;
                        edge_search_cache.insert({k, *leaf_itr});
                        continue;
                    }
                    else if (cache_itr != edge_search_cache.end())
                    {
                        tri_vert_indices[t] = *cache_itr;
                        this_cell->leaf->surface.insert({k, *cache_itr});
                        continue;
                    }

                    // Otherwise, perform the edge search here, storing
                    // the resulting position in both caches.
                    assert(va.inside != vb.inside);
                    const uint64_t surf_vert_index = va.inside
                        ? searchEdge(va.pos, vb.pos, this_cell->leaf->tape)
                        : searchEdge(vb.pos, va.pos, this_cell->leaf->tape);

                    this_cell->leaf->surface.insert({k, surf_vert_index});
                    edge_search_cache.insert({k, surf_vert_index});
                    tri_vert_indices[t] = surf_vert_index;
                }

                // Save the resulting triangle
                m.branes.push_back(tri_vert_indices);
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

}   // namespace Kernel
