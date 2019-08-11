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

#include "libfive/render/brep/simplex/simplex_debug.hpp"
#include "libfive/render/brep/simplex/simplex_tree.hpp"
#include "libfive/render/brep/indexes.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"

namespace libfive {

SimplexDebugMesher::SimplexDebugMesher(PerThreadBRep<3>& m, Tree t)
    : m(m), eval(new Evaluator(t)), owned(true)
{
    // Nothing to do here
}

SimplexDebugMesher::SimplexDebugMesher(PerThreadBRep<3>& m, Evaluator* es)
    : m(m), eval(es), owned(false)
{
    // Nothing to do here
}

SimplexDebugMesher::~SimplexDebugMesher() {
    if (owned) {
        delete eval;
    }
}

// Code copied from simplex_mesher.cpp, including comments
template <Axis::Axis A>
void SimplexDebugMesher::load(const std::array<const SimplexTree<3>*, 4>& ts)
{
    // Skip this if all of the cells are empty / filled
    if (std::all_of(ts.begin(), ts.end(),
        [](const SimplexTree<3>* t){ return t->type != Interval::AMBIGUOUS; }))
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
    };
    boost::container::static_vector<SubspaceVertex, 11> subvs;
    auto saveSubspaceVertex = [&subvs, &ts](unsigned index, NeighborIndex s) {
        assert(ts.at(index)->leaf != nullptr);
        assert(ts.at(index)->leaf->sub[s.i].load()->index.load() != 0);
        const auto sub = ts.at(index)->leaf->sub[s.i].load();
        subvs.push_back(SubspaceVertex {
            sub->vert,
            sub->index.load(),
            sub->inside,
        });
    };
    auto saveDummyVertex = [&subvs](Interval::State i) {
        subvs.push_back(SubspaceVertex {
                Eigen::Vector3d::Zero(),
                0,
                i == Interval::FILLED,
        });
    };

    {   /* First, we pick out the vertices on the common edge.  There are
         * three vertices associated with that edge: two on the corners,
         * and one on the edge itself. */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const SimplexTree<3>* a, const SimplexTree<3>* b)
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
            }
            if (!shared_okay) {
                continue;
            }

            // Now, build the tet as a set of edges
            for (unsigned j=0; j < 4; ++j) {
                for (unsigned k=j; k < 4; ++k) {
                    const auto a = subvs.at(vs.at(tet.at(j))).pos;
                    const auto b = subvs.at(vs.at(tet.at(k))).pos;
                    m.drawDebugLine(a.template cast<float>(), b.template cast<float>());
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//  Template initialization
template void SimplexDebugMesher::load<Axis::X>(
        const std::array<const SimplexTree<3>*, 4>&);
template void SimplexDebugMesher::load<Axis::Y>(
        const std::array<const SimplexTree<3>*, 4>&);
template void SimplexDebugMesher::load<Axis::Z>(
        const std::array<const SimplexTree<3>*, 4>&);

}   // namespace libfive

