/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <memory>

#include "libfive/render/brep/simplex/simplextree.hpp"
#include "libfive/render/brep/simplex/simplex_neighbors.hpp"
#include "libfive/render/brep/worker_pool.hpp"

namespace Kernel {

template <unsigned N>
using SimplexTreePool = WorkerPool<SimplexTree<N>, SimplexNeighbors<N>, N>;

}   // namespace Kernel

