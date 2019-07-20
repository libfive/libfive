/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "simplex_tree.cpp"
#include "../object_pool.cpp"

namespace Kernel {
template class SimplexTree<3>;
template struct SimplexLeaf<3>;
template class ObjectPool<SimplexTree<3>, SimplexTree<3>::Leaf,
                          SimplexLeafSubspace<3>>;
}   // namespace Kernel
