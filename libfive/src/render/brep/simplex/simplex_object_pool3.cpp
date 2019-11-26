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
template class ObjectPool<SimplexTree<3>, SimplexLeaf<3>,
                          SimplexLeafSubspace<3>>;
template class ObjectPool<SimplexLeaf<3>, SimplexLeafSubspace<3>>;
template class ObjectPool<SimplexLeafSubspace<3>>;

template SimplexTree<3>* ObjectPool<SimplexTree<3>, SimplexLeaf<3>,
                          SimplexLeafSubspace<3>>::get(
        libfive::SimplexTree<3>*, unsigned, Region<3>);
template SimplexLeaf<3>* ObjectPool<SimplexLeaf<3>,
                          SimplexLeafSubspace<3>>::get();
template SimplexLeafSubspace<3>* ObjectPool<SimplexLeafSubspace<3>>::get();
}   // namespace libfive
