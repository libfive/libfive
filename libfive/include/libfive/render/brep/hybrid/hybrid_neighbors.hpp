/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>

#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/indexes.hpp"
#include "libfive/render/brep/hybrid/hybrid_tree.hpp"

namespace Kernel {

template <unsigned N>
class HybridNeighbors :
    public Neighbors<N, HybridTree<N>, HybridNeighbors<N>>
{
public:
    /*  Constructor, returning an empty neighbor array */
    HybridNeighbors()
        : Neighbors<N, HybridTree<N>, HybridNeighbors<N>>()
    {
        // Nothing to do here
    }
};

}   // namespace Kernel


