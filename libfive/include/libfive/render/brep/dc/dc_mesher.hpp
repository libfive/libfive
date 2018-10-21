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

#include "libfive/render/brep/mesh.hpp"

namespace Kernel {

template <typename T, typename L> class Root;
class Mesh;

class DCMesher {
public:
    DCMesher(Mesh& m) : m(m) {}

    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    template <Axis::Axis A>
    void load(const std::array<const XTree<3>*, 4>& ts);


    /*  Walks an XTree, returning a mesh  */
    static std::unique_ptr<Mesh> mesh(
            const Root<XTree<3>, XTree<3>::Leaf>& tree,
            std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

protected:
    template <Axis::Axis A, bool D>
    void load(const std::array<const XTree<3>*, 4>& ts);

    Mesh& m;
};

}   // namespace Kernel
