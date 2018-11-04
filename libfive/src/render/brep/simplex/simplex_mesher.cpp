/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>
#include <fstream>

#include <boost/container/static_vector.hpp>

#include "libfive/render/brep/simplex/simplex_mesher.hpp"
#include "libfive/render/brep/simplex/simplextree.hpp"
#include "libfive/render/brep/simplex/solver.hpp"
#include "libfive/render/brep/indexes.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"

namespace Kernel {

SimplexMesher::SimplexMesher(PerThreadBRep<3>& m, Tree t)
    : m(m), eval(new XTreeEvaluator(t))
{
    // Nothing to do here
}

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
    // minimum-level cell to pull the subspace vertex from.

    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    /*  We'll be storing vertices in this array, in the following order:
     *  0:  edge
     *  1, 2: corners
     *  3: Face between ts[0] and ts[1]
     *  4: Face between ts[1] and ts[3]
     *  5: Face between ts[2] and ts[3]
     *  6: Face between ts[0] and ts[2]
     *  7: ts[0] cell
     *  8: ts[1] cell
     *  9: ts[3] cell
     *  10: ts[2] cell
     */
    struct SubspaceVertex {
        Eigen::Vector3d pos;
        uint64_t index;
        bool inside;
    };
    boost::container::static_vector<SubspaceVertex, 11> subvs;
    auto saveSubspaceVertex = [&subvs, &ts](unsigned index, NeighborIndex s) {
        assert(ts.at(index)->leaf != nullptr);
        assert(ts.at(index)->leaf->index.at(s.i) != 0);
        subvs.push_back(SubspaceVertex {
            ts.at(index)->leaf->vertices.row(s.i),
            ts.at(index)->leaf->index.at(s.i),
            ts.at(index)->leaf->inside.at(s.i),
        });
    };
    auto saveDummyVertex = [&subvs]() {
        subvs.push_back(SubspaceVertex {
                Eigen::Vector3d::Zero(),
                0,
                false,
        });
    };

    {   /* First, we pick out the vertices on the common edge.  There are
         * three vertices associated with that edge: two on the corners,
         * and one on the edge itself. */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const SimplexTree<3>* a, const SimplexTree<3>* b)
                { return a->leafLevel() < b->leafLevel(); }) - ts.begin();

        // All cells are EMPTY or FILLED, so we return early
        if (ts.at(index)->leafLevel() == SimplexTree<3>::LEAF_LEVEL_INVALID) {
            return;
        }
        assert(index != SimplexTree<3>::LEAF_LEVEL_INVALID);

        const CornerIndex corner_index_a(((index & 1) ? 0 : Q) |
                                         ((index & 2) ? 0 : R));
        const CornerIndex corner_index_b(corner_index_a.i ^ A);
        assert(ts.at(index)->leaf != nullptr);

        const auto corner_simplex_a = corner_index_a.neighbor();
        const auto corner_simplex_b = corner_index_b.neighbor();

        saveSubspaceVertex(index, corner_simplex_a | corner_simplex_b);
        saveSubspaceVertex(index, corner_simplex_a);
        saveSubspaceVertex(index, corner_simplex_b);
    }

    {   // Next, we'll find the vertices shared by faces
        struct FacePair {
            unsigned a;
            unsigned b;
            Axis::Axis axis;
        };
        const std::array<FacePair, 4> face_pairs =
            {{{0, 1, Q}, {1, 3, R}, {2, 3, Q}, {0, 2, R}}};

        for (const auto& p : face_pairs)
        {
            // Pull trees from the array
            auto ta = ts.at(p.a);
            auto tb = ts.at(p.b);

            if (ta->leafLevel() == tb->leafLevel() &&
                ta->leafLevel() == SimplexTree<3>::LEAF_LEVEL_INVALID)
            {
                saveDummyVertex();
            }
            else if (ta->leafLevel() < tb->leafLevel())
            {
                assert(ta->leafLevel() != SimplexTree<3>::LEAF_LEVEL_INVALID);
                saveSubspaceVertex(p.a, CornerIndex(p.axis).neighbor() |
                                        CornerIndex(7).neighbor());
            }
            else
            {
                // This handles the case where both vertices are at the same
                // level (and neither are EMPTY or FILLED).
                assert(tb->leafLevel() != SimplexTree<3>::LEAF_LEVEL_INVALID);
                saveSubspaceVertex(p.b, CornerIndex(0).neighbor() |
                                        CornerIndex(7 ^ p.axis).neighbor());
            }
        }
    }

    // Finally, we'll save the cell vertices
    // There's only one option per cell, since they aren't shared
    for (unsigned i : {0, 1, 3, 2})
    {
        if (ts[i]->type == Interval::AMBIGUOUS) {
            saveSubspaceVertex(i, ipow(3, 3) - 1);
        } else {
            saveDummyVertex();
        }
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
        {{0, 1, 2, 6, 3, 7}},  // Cell 0
        {{0, 1, 2, 3, 4, 8}},  // Cell 1
        {{0, 1, 2, 5, 6, 10}}, // Cell 3
        {{0, 1, 2, 4, 5, 9}},  // Cell 2
    }};

    //  Within each cell, indexing into the array above, here are the four
    //  tetrahedrons:
    const std::array<std::array<unsigned, 4>, 4> tet_vertices = {{
        {{0, 2, 3, 5}},
        {{0, 3, 1, 5}},
        {{0, 1, 4, 5}},
        {{0, 4, 2, 5}},
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
        {{ { 1,  2}, { 0,  2}, { 1,  3} }},
        {{ { 0,  3}, { 1,  3}, { 0,  2} }}, }},

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

        {{ // 0b1101
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
        return std::make_pair(std::min(a, b), std::max(a, b));
    };
    std::map<std::pair<uint64_t, uint64_t>, uint64_t> edge_search_cache;

    // Store all of the per-leaf edge caches into our cache here,
    // to prevent cases where we search an edge before getting to
    // the leaf that has already searched that edge.
    for (unsigned i=0; i < 4; ++i) {
        if (ts.at(i)->leaf != nullptr) {
            edge_search_cache.insert(ts.at(i)->leaf->surface.begin(),
                                     ts.at(i)->leaf->surface.end());
        }
    }

    // Iterate over the four cells
    for (unsigned i : {0, 1, 3, 2})
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
                mask |= subvs.at(vs.at(tet.at(j))).inside << j;
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
                    const auto& va = subvs.at(vs.at(tet.at(edge.first)));
                    const auto& vb = subvs.at(vs.at(tet.at(edge.second)));

                    // Confirm that we've assigned indices
                    assert(va.index != 0);
                    assert(vb.index != 0);

                    // Then build a globally unique key for this edge
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
                        assert(leaf_itr->second == cache_itr->second);
                        tri_vert_indices[t] = leaf_itr->second;
                        continue;
                    }
                    else if (leaf_itr != this_cell->leaf->surface.end())
                    {
                        tri_vert_indices[t] = leaf_itr->second;
                        edge_search_cache.insert({k, leaf_itr->second});
                        continue;
                    }
                    else if (cache_itr != edge_search_cache.end())
                    {
                        tri_vert_indices[t] = cache_itr->second;
                        this_cell->leaf->surface.insert({k, cache_itr->second});
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

uint64_t SimplexMesher::searchEdge(Eigen::Vector3d inside,
                                   Eigen::Vector3d outside,
                                   std::shared_ptr<Tape> tape)
{
    // This code is based on xtree.cpp, but flattened to a signle pass

    // There's an interesting question of precision + speed tradeoffs,
    // which mostly depend on how well evaluation scales in the
    // ArrayEaluator.  for now, we'll use the same value as XTree.
    constexpr int SEARCH_COUNT = 4;
    constexpr int POINTS_PER_SEARCH = 16;
    static_assert(POINTS_PER_SEARCH <= ArrayEvaluator::N,
                  "Overflowing ArrayEvaluator data array");

    // Multi-stage binary search for intersection
    for (int s=0; s < SEARCH_COUNT; ++s)
    {
        // Load search points into the evaluator
        Eigen::Array<double, 3, POINTS_PER_SEARCH> ps;
        for (int j=0; j < POINTS_PER_SEARCH; ++j)
        {
                const double frac = j / (POINTS_PER_SEARCH - 1.0);
                ps.col(j) = (inside * (1 - frac)) +
                            (outside * frac);
                eval->array.set(ps.col(j).template cast<float>(), j);
        }

        auto out = eval->array.values(POINTS_PER_SEARCH, tape);

        // Skip one point, because the very first point is
        // already known to be inside the shape (but
        // sometimes, due to numerical issues, it registers
        // as outside!)
        for (unsigned j=1; j < POINTS_PER_SEARCH; ++j)
        {
            // We're searching for the first point that's outside of the
            // surface.  There's a special case for the final point in the
            // search, working around  numerical issues where different
            // evaluators disagree with whether points are inside or outside.
            if (out[j] > 0 || j == POINTS_PER_SEARCH - 1 ||
                (out[j] == 0 && !eval->feature.isInside(
                            ps.col(j).template cast<float>(), tape)))

            {
                inside = ps.col(j - 1);
                outside = ps.col(j);
                break;
            }
        }
    }

    // TODO: we should weight the exact position based on values
    Eigen::Vector3d vert = (inside + outside) / 2;
    return m.pushVertex(vert.template cast<float>());
}

////////////////////////////////////////////////////////////////////////////////
//  Template initialization
template void SimplexMesher::load<Axis::X>(
        const std::array<const SimplexTree<3>*, 4>&);
template void SimplexMesher::load<Axis::Y>(
        const std::array<const SimplexTree<3>*, 4>&);
template void SimplexMesher::load<Axis::Z>(
        const std::array<const SimplexTree<3>*, 4>&);

}   // namespace Kernel
