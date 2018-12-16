/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <atomic>
#include <boost/lockfree/stack.hpp>

#include "libfive/eval/eval_xtree.hpp"
#include "libfive/render/brep/root.hpp"
#include "libfive/render/brep/progress.hpp"

namespace Kernel {

/*
 *  A WorkerPool is used to construct a recursive tree (quadtree / octree)
 *  by sharing the work among a pool of threads.
 */
template <typename T, typename Neighbors, unsigned N>
class WorkerPool
{
public:
    /*
     *  Builder function with optional arguments, fewer features
     */
    static Root<T> build(const Tree t, Region<N> region,
            double min_feature=0.1, double max_err=1e-8, unsigned workers=8,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

    /*
     *  Full-featured builder function
     */
    static Root<T> build(XTreeEvaluator* eval,
            Region<N> region, double min_feature,
            double max_err, unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

protected:
    struct Task {
        T* target;
        std::shared_ptr<Tape> tape;
        Region<N> region;
        Neighbors parent_neighbors;
    };

    using LockFreeStack =
        boost::lockfree::stack<Task, boost::lockfree::fixed_sized<true>>;

    static void run(XTreeEvaluator* eval, LockFreeStack& tasks,
                    const float min_feature, const float max_err,
                    std::atomic_bool& done, std::atomic_bool& cancel,
                    Root<T>& root, std::mutex& root_lock,
                    ProgressWatcher* progress);
};

}   // namespace Kernel
