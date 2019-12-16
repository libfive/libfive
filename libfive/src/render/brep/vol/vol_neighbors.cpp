/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/vol/vol_neighbors.hpp"
#include "libfive/render/brep/vol/vol_tree.hpp"
#include "../neighbors.inl"

namespace libfive {

VolNeighbors::VolNeighbors()
    : Neighbors<3, VolTree, VolNeighbors>()
{
    // Nothing to do here
}

// Explicit template instantiation
template class Neighbors<3, VolTree, VolNeighbors>;

}   // namespace libfive
