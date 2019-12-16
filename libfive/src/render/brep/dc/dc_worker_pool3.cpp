/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "../worker_pool.inl"
#include "libfive/render/brep/dc/dc_worker_pool.hpp"

namespace libfive {
template class WorkerPool<DCTree<3>, DCNeighbors<3>, 3>;
}
