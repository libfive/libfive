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
template class ObjectPool<DCTree<3>, DCLeaf<3>, Intersection<3>>;
template class ObjectPool<DCLeaf<3>, Intersection<3>>;
template class ObjectPool<Intersection<3>>;

template DCTree<3>* ObjectPool<DCTree<3>, DCLeaf<3>, Intersection<3>>::get(
        DCTree<3>*, unsigned, Region<3>);
template DCLeaf<3>* ObjectPool<DCLeaf<3>, Intersection<3>>::get();
template Intersection<3>* ObjectPool<Intersection<3>>::get();
}   // namespace libfive
