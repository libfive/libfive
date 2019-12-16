/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "../neighbors.inl"
#include "libfive/render/brep/hybrid/hybrid_neighbors.hpp"

namespace libfive {
template class Neighbors<2, HybridTree<2>, HybridNeighbors<2>>;
template class HybridNeighbors<2>;
}   // namespace libfive
