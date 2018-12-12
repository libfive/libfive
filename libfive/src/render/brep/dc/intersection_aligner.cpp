/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/dc/intersection_aligner.hpp"
#include "libfive/render/brep/dc/dc_mesher.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

#if LIBFIVE_TRIANGLE_FAN_MESHING

template <Axis::Axis A>
void IntersectionAligner::load(const std::array<const DCTree<3>*, 4>& ts)
{
    auto index_sign = DCMesher::getIndexAndSign<A>(ts);
    if (index_sign.first == -1) {
        return;
    } else {
        assert(index_sign.first >= 0);
    }

    if (index_sign.second) {
        load<A, 1>(ts, index_sign.first);
    } else {
        load<A, 0>(ts, index_sign.first);
    }
}

template <Axis::Axis A, bool D>
void IntersectionAligner::load(
    const std::array<const DCTree<3>*, 4>& ts, unsigned index)
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
        for (unsigned i = 0; i < 4; ++i)
        {
            es[i] = MarchingTable<3>::mt.e[D ? ev[i].first : ev[i].second]
                [D ? ev[i].second : ev[i].first];
            assert(es[i] != -1);
        }
    }
    // We want to make every t's edge match index's, but if there's a duplicate
    // t, then the edge in question is not an edge of it, so we do not want to
    // use it.
    const auto& source = ts[index]->intersection(es[index]);
    auto noDuplicates = true;
    auto setDest = [&source, &ts, &es](int position)
    {
        ts[position]->setIntersectionPtr(es[position], source);
    };
    for (auto i = 1; i < 3; ++i)
    {
        if (ts[index ^ i] == ts[index ^ 3])
        {
            noDuplicates = false;
        }
        else
        {
            setDest(index ^ i);
        }
    }
    if (noDuplicates)
    {
        setDest(index ^ 3);
    }
}

// Explicit template instantiation
template void IntersectionAligner::load<Axis::X>(const std::array<const DCTree<3>*, 4>&);
template void IntersectionAligner::load<Axis::Y>(const std::array<const DCTree<3>*, 4>&);
template void IntersectionAligner::load<Axis::Z>(const std::array<const DCTree<3>*, 4>&);

#endif

}   // namespace Kernel
