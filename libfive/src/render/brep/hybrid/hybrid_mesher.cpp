/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>
#include <fstream>

#include <boost/container/static_vector.hpp>

#include "libfive/eval/evaluator.hpp"

#include "libfive/render/brep/hybrid/hybrid_mesher.hpp"
#include "libfive/render/brep/hybrid/hybrid_tree.hpp"
#include "libfive/render/brep/simplex/surface_edge_map.hpp"
#include "libfive/render/brep/indexes.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"

namespace libfive {

HybridMesher::HybridMesher(PerThreadBRep<3>& m, Tree t)
    : m(m), eval(new Evaluator(t)), owned(true)
{
    // Nothing to do here
}

HybridMesher::HybridMesher(PerThreadBRep<3>& m, Evaluator* es)
    : m(m), eval(es), owned(false)
{
    // Nothing to do here
}

HybridMesher::~HybridMesher() {
    if (owned) {
        delete eval;
    }
}

template <Axis::Axis A>
void HybridMesher::load(const std::array<const HybridTree<3>*, 4>& ts)
{
    // Skip this if all of the cells are empty / filled
    if (std::all_of(ts.begin(), ts.end(),
        [](const HybridTree<3>* t){ return t->type != Interval::AMBIGUOUS; }))
    {
        return;
    }

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
        bool on_surface;
    };
    boost::container::static_vector<SubspaceVertex, 11> subvs;
    auto saveSubspaceVertex = [&subvs, &ts](unsigned index, NeighborIndex s) {
        assert(ts.at(index)->leaf != nullptr);
        assert(ts.at(index)->leaf->index[s.i] != 0);
        const auto leaf = ts.at(index)->leaf;
        subvs.push_back(SubspaceVertex {
            leaf->vertex_pos.col(s.i),
            leaf->index[s.i],
            leaf->inside[s.i],
            leaf->vertex_on_surface[s.i],
        });
    };
    auto saveDummyVertex = [&subvs](Interval::State i) {
        subvs.push_back(SubspaceVertex {
                Eigen::Vector3d::Zero(),
                0,
                i == Interval::FILLED,
                false,
        });
    };

    {   /* First, we pick out the vertices on the common edge.  There are
         * three vertices associated with that edge: two on the corners,
         * and one on the edge itself. */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const HybridTree<3>* a, const HybridTree<3>* b)
                { return a->leafLevel() < b->leafLevel(); }) - ts.begin();

        // Sanity checking to make sure that this edge belongs to a
        // valid leaf.  If one of these fails, all should fail, but
        // better safe than sorry.
        assert(ts.at(index)->type != Interval::UNKNOWN);
        assert(ts.at(index)->leafLevel() != UINT32_MAX);
        assert(ts.at(index)->leaf != nullptr);

        const CornerIndex corner_index_a(((index & 1) ? 0 : Q) |
                                         ((index & 2) ? 0 : R));
        const CornerIndex corner_index_b(corner_index_a.i ^ A);

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

            // This case only happens at the top of the tree, when we walk
            // the edges of the top cell with dummy cells around it.
            if (ta->type == Interval::UNKNOWN &&
                tb->type == Interval::UNKNOWN)
            {
                saveDummyVertex(ta->type);
            }
            else if (ta->leafLevel() < tb->leafLevel())
            {
                saveSubspaceVertex(p.a, CornerIndex(p.axis).neighbor() |
                                        CornerIndex(7).neighbor());
            }
            else if (ta->leafLevel() > tb->leafLevel())
            {
                saveSubspaceVertex(p.b, CornerIndex(0).neighbor() |
                                        CornerIndex(7 ^ p.axis).neighbor());
            }
            // This is the same as the case above, but is written out
            // for clarity: if both leafs are at the same level, then it
            // doesn't matter which vertex is saved, so we pick the second.
            else
            {
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
            saveDummyVertex(ts[i]->type);
        }
    }

    // Now that we've populated our vertex table, we walk around the four cells
    // and perform marching tetrahedrons on 4 tets within each cell.
    //
    // First, we define which six vertices we care about in each cell,
    // as indices into the subvs array in the order
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
        {{0, 1, 2, 5, 6, 10}}, // Cell 2
        {{0, 1, 2, 4, 5, 9}},  // Cell 3
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
    SurfaceEdgeMap<128> edge_search_cache;

    // Store all of the per-leaf edge caches into our cache here,
    // to prevent cases where we search an edge before getting to
    // the leaf that has already searched that edge.
    for (unsigned i=0; i < 4; ++i) {
        const auto leaf = ts.at(i)->leaf;
        if (leaf != nullptr) {
            for (unsigned j=0; j < leaf->surface.size(); ++j) {
                edge_search_cache.insert(leaf->surface.key(j),
                                         leaf->surface.value(j));
            }
        }
    }

    // Iterate over the four cells
    const std::array<unsigned, 4> order = {{0, 1, 3, 2}};
    for (unsigned index=0; index < 4; ++index)
    {
        const auto i = order.at(index);
        const auto this_cell = ts.at(i);

        // Skip empty, filled, and dummy cells immediately
        if (this_cell->leaf == nullptr) {
            assert(this_cell->type == Interval::UNKNOWN);
            continue;
        } else if (this_cell->type == Interval::EMPTY ||
                   this_cell->type == Interval::FILLED)
        {
            continue;
        }

        // We skip cells which don't actually contain the edge, which is
        // detected by seeing if they're duplicated from a neighbor as
        // we rotate around the axis.
        //
        // Here's an ASCII-art depiction:
        //
        //   ------W--------
        //   |    /| B |   |
        //   |  C -V-------|
        //   |    \| A |   |
        //   ---------------
        //   If this function is called with cells A, B, C, C about axis V,
        //   then we only want to generate two triangles within cell C,
        //   rather than 4 triangles (which would be generated in the naive
        //   case, where cell C is treated as both lower-left and upper-left).
        //
        //   We store whether the previous or next cell (clockwise) is the
        //   same as this cell, then check whether that face is used in the
        //   tet below (skipping the tet if that is the case).
        const bool next_shared =
            (this_cell == ts.at(order.at((index + 1) % 4)));
        const bool prev_shared =
            (this_cell == ts.at(order.at((index + 3) % 4)));

        // Iterate over the six tetrahedrons
        const auto& vs = cell_vertices.at(i);
        for (auto& tet: tet_vertices)
        {
            // Build the corner mask
            unsigned mask = 0;
            bool shared_okay = true;
            for (unsigned j=0; j < 4; ++j) {
                // This is the check described above, where we
                // skip tets which use a face that's invalid (due
                // to a duplicate, larger cell being included twice).
                //
                // tet vertices 3 and 4 are the face-shared vertices,
                // which is why we check those numbers specifically.
                if ((tet.at(j) == 3 && prev_shared) ||
                    (tet.at(j) == 4 && next_shared))
                {
                    shared_okay = false;
                }
                mask |= subvs.at(vs.at(tet.at(j))).inside << j;
            }
            if (!shared_okay) {
                continue;
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

                    //  Everything in the leaf cache has been moved into
                    //  edge_search_cache, so we only need to look for the
                    //  index in a single place.
                    const auto cache_index = edge_search_cache.find(k);
                    if (cache_index)
                    {
                        tri_vert_indices[t] = cache_index;

                        // Re-inserting into the leaf's map, since insertion
                        // stops if the value is already found, so this is
                        // cheaper than checking if it's present first
                        this_cell->leaf->surface.insert(k, cache_index);
                    }
                    else
                    {
                        // Otherwise, perform the edge search here, storing
                        // the resulting position in both caches.
                        assert(va.inside != vb.inside);

                        uint64_t surf_vert_index;
                        if (va.on_surface && !vb.on_surface) {
                            surf_vert_index = m.pushVertex(va.pos);
                        } else if (vb.on_surface && !va.on_surface) {
                            surf_vert_index = m.pushVertex(vb.pos);
                        } else if (va.on_surface && vb.on_surface) {
                            const Eigen::Vector3d c = (va.pos + vb.pos) / 2;
                            surf_vert_index = m.pushVertex(c);
                        } else {
                            surf_vert_index = va.inside
                                ? searchEdge(va.pos, vb.pos, this_cell->leaf->tape)
                                : searchEdge(vb.pos, va.pos, this_cell->leaf->tape);
                        }

                        this_cell->leaf->surface.insert(k, surf_vert_index);
                        edge_search_cache.insert(k, surf_vert_index);
                        tri_vert_indices[t] = surf_vert_index;
                    }
                }

                // Save the resulting triangle
                m.branes.push_back(tri_vert_indices);
            }
        }
    }
}

uint64_t HybridMesher::searchEdge(Eigen::Vector3d inside,
                                  Eigen::Vector3d outside,
                                  const std::shared_ptr<Tape>& tape)
{
    // This code is based on xtree.cpp, but flattened to a signle pass
    assert(tape.get() != nullptr);

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
                eval->set(ps.col(j).template cast<float>(), j);
        }

        auto out = eval->values(POINTS_PER_SEARCH, *tape);

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
                (out[j] == 0 && !eval->isInside(
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

    return m.pushVertex(vert);
}

////////////////////////////////////////////////////////////////////////////////
//  Template initialization
template void HybridMesher::load<Axis::X>(
        const std::array<const HybridTree<3>*, 4>&);
template void HybridMesher::load<Axis::Y>(
        const std::array<const HybridTree<3>*, 4>&);
template void HybridMesher::load<Axis::Z>(
        const std::array<const HybridTree<3>*, 4>&);

}   // namespace libfive

