/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/axes.hpp"
#include "libfive/render/brep/brep.hpp"

namespace Kernel {

// Forward declaration
template <unsigned N> class SimplexTree;

class SimplexMesher
{
    /*
     *  Called by Dual::walk to construct the triangle mesh
     */
    template <Axis::Axis A>
    void load(const std::array<const SimplexTree<3>*, 4>& ts);
};

}   // namespace Kernel
