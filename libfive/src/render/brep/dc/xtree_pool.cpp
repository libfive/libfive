/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <future>
#include <numeric>
#include <functional>
#include <limits>
#include <stack>
#include <chrono>

#include <cmath>

#include <boost/lockfree/stack.hpp>

#include "libfive/render/brep/dc/xtree.hpp"
#include "libfive/render/brep/dc/xtree_pool.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/worker_pool.hpp"
#include "libfive/eval/tape.hpp"

namespace Kernel {

template <unsigned N>
Root<XTree<N>, typename XTree<N>::Leaf> XTreePool<N>::build(
            const Tree t, Region<N> region,
            double min_feature, double max_err, unsigned workers,
            ProgressCallback progress_callback)
{
    std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
    es.reserve(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        es.emplace_back(XTreeEvaluator(t));
    }
    std::atomic_bool cancel(false);
    return XTreePool<N>::build(es.data(), region, min_feature,
                               max_err, workers, cancel, progress_callback);
}

template <unsigned N>
Root<XTree<N>, typename XTree<N>::Leaf> XTreePool<N>::build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback)
{
    // Lazy initialization of marching squares / cubes table
    if (XTree<N>::mt.get() == nullptr)
    {
        XTree<N>::mt = Marching::buildTable<N>();
    }
    return WorkerPool<XTree<N>, typename XTree<N>::Leaf,
                      DCNeighbors<N>, N>::build(
        eval, region, min_feature, max_err, workers, cancel, progress_callback);
}

}   // namespace Kernel
