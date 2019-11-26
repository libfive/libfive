/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/simplex/simplex_tree.hpp"
#include "../object_pool.inl"

namespace libfive {
template class ObjectPool<SimplexTree<2>, SimplexLeaf<2>,
                          SimplexLeafSubspace<2>>;
template class ObjectPool<SimplexLeaf<2>, SimplexLeafSubspace<2>>;
template class ObjectPool<SimplexLeafSubspace<2>>;

template SimplexTree<2>* ObjectPool<SimplexTree<2>, SimplexLeaf<2>,
                          SimplexLeafSubspace<2>>::get(
        libfive::SimplexTree<2>*, unsigned, Region<2>);
template SimplexLeaf<2>* ObjectPool<SimplexLeaf<2>,
                          SimplexLeafSubspace<2>>::get();
template SimplexLeafSubspace<2>* ObjectPool<SimplexLeafSubspace<2>>::get();
}   // namespace libfive
