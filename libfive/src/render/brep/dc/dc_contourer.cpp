/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/dc/dc_contourer.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"

namespace libfive {

template <Axis::Axis A>
void DCContourer::load(const std::array<const DCTree<2>*, 2>& ts)
{
    // Exit immediately if we can prove that there will be no
    // face produced by this edge.
    if (std::any_of(ts.begin(), ts.end(),
        [](const DCTree<2>* t){ return t->type != Interval::AMBIGUOUS; }))
    {
        return;
    }

    // Sanity-checking that all cells have a Leaf struct allocated
    for (auto& t : ts)
    {
        assert(t->leaf != nullptr);
        (void)t;
    }

    /*  See comment in mesh.cpp about selecting a minimum edge */
    const auto index = std::min_element(ts.begin(), ts.end(),
            [](const DCTree<2>* a, const DCTree<2>* b)
            { return a->leaf->level < b->leaf->level; }) - ts.begin();

    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;
    constexpr std::array<uint8_t, 2> corners = {{perp, 0}};

    // If there is a sign change across the relevant edge, then call the
    // watcher with the segment corners (with proper winding order)
    auto a = ts[index]->cornerState(corners[index]);
    auto b = ts[index]->cornerState(corners[index] | A);
    if (a != b)
    {
        // Use either forward or reversed segment building
        if ((a == Interval::FILLED && A == Axis::Y) ||
            (b == Interval::FILLED && A == Axis::X))
        {
            load<A, 0>(ts);
        }
        else
        {
            load<A, 1>(ts);
        }
    }
}


template <Axis::Axis A, bool D>
void DCContourer::load(const std::array<const DCTree<2>*, 2>& ts)
{
    // From axis and contour direction, extract the relevant edge index
    // numbers for the two cells in ts
    int es[2];
    if (D ^ (A == Axis::X))
    {
        es[0] = MarchingTable<2>::e(3)[3^A];
        es[1] = MarchingTable<2>::e(A)[0];
    }
    else
    {
        es[0] = MarchingTable<2>::e(3^A)[3];
        es[1] = MarchingTable<2>::e(0)[A];
    }
    assert(es[0] != -1);
    assert(es[1] != -1);

    uint32_t vs[2];
    for (unsigned i=0; i < ts.size(); ++i)
    {
        assert(ts[i]->leaf != nullptr);

        auto vi = ts[i]->leaf->level > 0
            ? 0
            : MarchingTable<2>::p(ts[i]->leaf->corner_mask)[es[i]];
        assert(vi != -1);

        // Sanity-checking manifoldness of collapsed cells
        assert(ts[i]->leaf->level == 0 || ts[i]->leaf->vertex_count == 1);

        if (ts[i]->leaf->index[vi] == 0)
        {
            ts[i]->leaf->index[vi] = m.pushVertex(ts[i]->vert(vi));
        }
        vs[i] = ts[i]->leaf->index[vi];
    }
    // Handle contour winding direction
    m.branes.push_back({vs[!D], vs[D]});
}

////////////////////////////////////////////////////////////////////////////////

// Explicit template instantiation
template void DCContourer::load<Axis::X>(const std::array<const DCTree<2>*, 2>&);
template void DCContourer::load<Axis::Y>(const std::array<const DCTree<2>*, 2>&);
template void DCContourer::load<Axis::Z>(const std::array<const DCTree<2>*, 2>&);

}   // namespace libfive
