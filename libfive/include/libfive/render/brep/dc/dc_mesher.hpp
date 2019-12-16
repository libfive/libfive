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

namespace libfive {

/* Forward declarations */
template <unsigned N> class PerThreadBRep;
template <unsigned N> class DCTree;
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
     *  DC meshing doesn't need to handle the top edges of the tree
     */
    static bool needsTopEdges() { return false; }

protected:
    template <Axis::Axis A, bool D>
    void load(const std::array<const DCTree<3>*, 4>& ts);

    PerThreadBRep<3>& m;
};

}   // namespace libfive
