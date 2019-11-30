
/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#ifdef FIVE_TBB
#include "../worker_pool_tbb.cpp"
#else
#include "../worker_pool.cpp"
#endif

#include "libfive/render/brep/simplex/simplex_worker_pool.hpp"

namespace libfive {
template class WorkerPool<SimplexTree<3>, SimplexNeighbors<3>, 3>;
}
