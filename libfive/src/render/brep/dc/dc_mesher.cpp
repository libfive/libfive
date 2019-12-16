/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>

#include "libfive/render/brep/dc/dc_mesher.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"

namespace libfive {

template <Axis::Axis A>
void DCMesher::load(const std::array<const DCTree<3>*, 4>& ts)
{
    // Exit immediately if we can prove that there will be no
    // face produced by this edge.
    if (std::any_of(ts.begin(), ts.end(),
        [](const DCTree<3>* t){ return t->type != Interval::AMBIGUOUS; }))
    {
        return;
    }

    // Sanity-checking that all cells have a Leaf struct allocated
    for (auto& t : ts)
    {
        assert(t->leaf != nullptr);
        (void)t;
    }

    /*  We need to check the values on the shared edge to see whether we need
     *  to add a face.  However, this is tricky when the edge spans multiple
     *  octree levels.
     *
     * In the following diagram, the target edge is marked with an o
     * (travelling out of the screen):
     *      _________________
     *      | 2 |           |
     *      ----o   1, 3    |  ^ R
     *      | 0 |           |  |
     *      ----------------|  --> Q
     *
     *  If we were to look at corners of c or d, we wouldn't be looking at the
     *  correct edge.  Instead, we need to look at corners for the smallest cell
     *  among the function arguments.
     */
    const auto index = std::min_element(ts.begin(), ts.end(),
            [](const DCTree<3>* a, const DCTree<3>* b)
            { return a->leaf->level < b->leaf->level; }) - ts.begin();

    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    constexpr std::array<uint8_t, 4> corners = {{Q|R, R, Q, 0}};

    // If there is a sign change across the relevant edge, then call the
    // watcher with the segment corners (with proper winding order)
    auto a = ts[index]->cornerState(corners[index]);
    auto b = ts[index]->cornerState(corners[index] | A);
    if (a != b)
    {
        if (a != Interval::FILLED) {
            load<A, 0>(ts);
        } else {
            load<A, 1>(ts);
        }
    }
}

template <Axis::Axis A, bool D>
void DCMesher::load(const std::array<const DCTree<3>*, 4>& ts)
{
    int es[4];
    {   // Unpack edge vertex pairs into edge indices
        auto q = Axis::Q(A);
        auto r = Axis::R(A);
        std::vector<std::pair<unsigned, unsigned>> ev = {
            {q | r, q | r | A},
            {r, r | A},
            {q, q | A},
            {0, A} };
        for (unsigned i=0; i < 4; ++i)
        {
            es[i] = MarchingTable<3>::e(D ? ev[i].first  : ev[i].second)
                                   [D ? ev[i].second : ev[i].first];
            assert(es[i] != -1);
        }
    }

    uint32_t vs[4];
    Eigen::Matrix<float, 3, 4> vert_positions;
    for (unsigned i=0; i < ts.size(); ++i)
    {
        assert(ts[i]->leaf != nullptr);

        // Load either a patch-specific vertex (if this is a lowest-level,
        // potentially non-manifold cell) or the default vertex
        auto vi = ts[i]->leaf->level > 0
            ? 0
            : MarchingTable<3>::p(ts[i]->leaf->corner_mask)[es[i]];
        assert(vi != -1);

        // Sanity-checking manifoldness of collapsed cells
        assert(ts[i]->leaf->level == 0 || ts[i]->leaf->vertex_count == 1);

        if (ts[i]->leaf->index[vi] == 0)
        {
            ts[i]->leaf->index[vi] = m.pushVertex(ts[i]->vert(vi));
        }

        // Save the vertex position for normal calculations
        vert_positions.col(i) = ts[i]->vert(vi).template cast<float>();

        // Store the vertex index for pushing triangles
        vs[i] = ts[i]->leaf->index[vi];
    }

    // Handle polarity-based windings
    if (!D)
    {
        std::swap(vs[1], vs[2]);

        const Eigen::Vector3f r = vert_positions.col(1);
        vert_positions.col(1) = vert_positions.col(2);
        vert_positions.col(2) = r;
    }
    // Pick a triangulation that prevents triangles from folding back
    // on each other by checking normals.
    std::array<Eigen::Vector3f, 4> norms;

    // Computes and saves a corner normal.  a,b,c must be right-handed
    // according to the quad winding, which looks like
    //     2---------3
    //     |         |
    //     |         |
    //     0---------1
    auto saveNorm = [&](int a, int b, int c){
        norms[a] = (vert_positions.col(b) - vert_positions.col(a)).cross
                   (vert_positions.col(c) - vert_positions.col(a)).normalized();
    };
    saveNorm(0, 1, 2);
    saveNorm(1, 3, 0);
    saveNorm(2, 0, 3);
    saveNorm(3, 2, 1);

    // Helper function to push triangles that aren't simply lines
    auto push_triangle = [&](uint32_t a, uint32_t b, uint32_t c) {
        if (a != b && b != c && a != c)
        {
            m.branes.push_back({a, b, c});
        }
    };

    if (norms[0].dot(norms[3]) > norms[1].dot(norms[2]))
    {
        push_triangle(vs[0], vs[1], vs[2]);
        push_triangle(vs[2], vs[1], vs[3]);
    }
    else
    {
        push_triangle(vs[0], vs[1], vs[3]);
        push_triangle(vs[0], vs[3], vs[2]);
    }
}

////////////////////////////////////////////////////////////////////////////////

// Explicit template instantiation
template void DCMesher::load<Axis::X>(const std::array<const DCTree<3>*, 4>&);
template void DCMesher::load<Axis::Y>(const std::array<const DCTree<3>*, 4>&);
template void DCMesher::load<Axis::Z>(const std::array<const DCTree<3>*, 4>&);

}   // namespace libfive
