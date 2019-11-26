/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "../neighbors.inl"
#include "simplex_neighbors.inl"

namespace libfive {
template class Neighbors<3, SimplexTree<3>, SimplexNeighbors<3>>;
template class SimplexNeighbors<3>;
}   // namespace libfive


