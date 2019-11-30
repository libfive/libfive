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

#include "libfive/render/brep/root.hpp"
#include "libfive/tree/tree.hpp"

#include <tbb/enumerable_thread_specific.h>

namespace libfive {

// Forward declarations
class Evaluator;
template <unsigned N> class Region;
class Tape;
struct BRepSettings;
class VolTree;

/*
 *  A WorkerPool is used to construct a recursive tree (quadtree / octree)
 *  by sharing the work among a pool of threads.
 */
template <typename T, typename Neighbors, unsigned N>
class WorkerPool
{
public:
    /*
     *  Evaluation function that builds a local evaluator array
     *  (based on settings.workers)
     */
    static Root<T> build(Tree t, const Region<N>& region,
                         const BRepSettings& settings);
    /*
     *  General-purpose evaluation function
     *
     *  eval must be an array of at least [settings.workers] evaluators
     */
    static Root<T> build(tbb::enumerable_thread_specific<Evaluator>& evaluators,
                         const Region<N>& region,
                         const BRepSettings& settings);

    struct Task {
        T* target;
        std::shared_ptr<Tape> tape;
        Neighbors parent_neighbors;
        const VolTree* vol;
    };

protected:
    using LockFreeStack =
        boost::lockfree::stack<Task, boost::lockfree::fixed_sized<true>>;
};

}   // namespace libfive
