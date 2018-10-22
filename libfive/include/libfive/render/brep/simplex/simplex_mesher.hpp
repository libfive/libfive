/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/axes.hpp"
#include "libfive/render/brep/mesh.hpp"

namespace Kernel {

// Forward declaration
template <unsigned N> class SimplexTree;

class SimplexMesher
{
public:
    SimplexMesher(Mesh& m, XTreeEvaluator* eval) : m(m), eval(eval) {}

    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    template <Axis::Axis A>
    void load(const std::array<const SimplexTree<3>*, 4>& ts);

protected:
    /*
     *  Performs a binary search along a particular edge using the
     *  provided tape.  Stores the resulting vertex into the Mesh m,
     *  and returns its index.
     */
    uint64_t searchEdge(const Eigen::Vector3d& inside,
                        const Eigen::Vector3d& outside,
                        std::shared_ptr<Tape> tape);

    Mesh& m;
    XTreeEvaluator* eval;
};

}   // namespace Kernel
