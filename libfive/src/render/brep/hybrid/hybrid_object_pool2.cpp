/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/hybrid/hybrid_tree.hpp"
#include "../object_pool.inl"

namespace libfive {
template class ObjectPool<HybridTree<2>, HybridLeaf<2>>;
template HybridTree<2>* ObjectPool<HybridTree<2>, HybridLeaf<2>>::get(
        libfive::HybridTree<2>*, unsigned, Region<2>);
}   // namespace libfive
