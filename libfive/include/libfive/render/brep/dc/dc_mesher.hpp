/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include "libfive/render/axes.hpp"

#include "libfive/render/brep/per_thread_brep.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"

#ifndef LIBFIVE_TRIANGLE_FAN_MESHING
#define LIBFIVE_TRIANGLE_FAN_MESHING 1
#endif

namespace Kernel {

template <typename T> class Root;
class Mesh;

class DCMesher {
public:
    using Output = Mesh;
    using Input = DCTree<3>;

    DCMesher(PerThreadBRep<3>& m) : m(m)
        {   /* Nothing to do here */    }

    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    template <Axis::Axis A>
    void load(const std::array<const DCTree<3>*, 4>& ts);

    /*
     *  Returns the index of the smallest tree, and the sign of the edge
     *  spanning that smallest tree.  This is used in various classes
     *  that do DCMesher-type things, but can be called independently.
     *
     *  Returns {-1, false} if the parent caller should abort (because not
     *  all of the child trees are ambiguous).
     */
    template <Axis::Axis A>
    static std::pair<int, bool> getIndexAndSign(
            const std::array<const DCTree<3>*, 4>& ts);

protected:
    template <Axis::Axis A, bool D>
    void load(const std::array<const DCTree<3>*, 4>& ts, unsigned index);

#if LIBFIVE_TRIANGLE_FAN_MESHING
    /*
     *  It is possible for a triangle to escape the cells that generated it,
     *  if said cells are of different sizes.  This function adds branes in
     *  such a way that, so long as the endpoints are in their proper cells,
     *  the entire triangle will be as well.  The axis and direction are not
     *  those of the load function that calls this, but rather from a to b.
     *  a and b must be ordered such that aIndex, bIndex, intersectionIndex
     *  is the proper winding for this triangle.
     */
    template <Axis::Axis A, bool D>
    void checkAndAddTriangle(const DCTree<3>* a, const DCTree<3>* b,
                             uint32_t aIndex, uint32_t bIndex,
                             uint32_t intersectionIndex);

    /*
     *  Used to store the indices of vertices created by forcing triangles to
     *  be inside the cells generating them.  The key values are the
     *  corresponding cell vertices in ascending order, and the resulting
     *  vertex may or may not be an intersection vertex.
     */
    std::map<std::pair<uint32_t, uint32_t>, uint32_t> forcedVerts;
#endif

    PerThreadBRep<3>& m;
};

}   // namespace Kernel
