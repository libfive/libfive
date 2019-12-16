/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "dc_tree.inl"
#include "../object_pool.inl"

namespace libfive {

template <>
bool DCTree<2>::cornersAreManifold(const uint8_t corner_mask)
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
template <>
bool DCTree<2>::leafsAreManifold(
        const std::array<DCTree<2>*, 1 << 2>& cs,
        const std::array<Interval::State, 1 << 2>& corners)
{
    /*  See detailed comment in Octree::leafsAreManifold */
    const bool edges_safe =
        (cs[0]->cornerState(Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::X) == corners[Axis::X])
    &&  (cs[0]->cornerState(Axis::Y) == corners[0] ||
         cs[0]->cornerState(Axis::Y) == corners[Axis::Y])
    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X|Axis::Y])
    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    const bool faces_safe =
        (cs[0]->cornerState(Axis::Y|Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    return edges_safe && faces_safe;
}

// Explicit initialization of templates
template class DCTree<2>;
template struct DCLeaf<2>;

}   // namespace libfive
