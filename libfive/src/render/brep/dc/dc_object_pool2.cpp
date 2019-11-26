/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/dc/dc_tree.hpp"
#include "../object_pool.inl"

namespace libfive {
template class ObjectPool<DCTree<2>, DCLeaf<2>, Intersection<2>>;
template class ObjectPool<DCLeaf<2>, Intersection<2>>;
template class ObjectPool<Intersection<2>>;

template DCTree<2>* ObjectPool<DCTree<2>, DCLeaf<2>, Intersection<2>>::get(
        DCTree<2>*, unsigned, Region<2>);
template DCLeaf<2>* ObjectPool<DCLeaf<2>, Intersection<2>>::get();
template Intersection<2>* ObjectPool<Intersection<2>>::get();
}   // namespace libfive
