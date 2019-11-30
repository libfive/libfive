/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/worker_pool_tbb.hpp"
#include "libfive/render/brep/vol/vol_tree.hpp"
#include "libfive/eval/evaluator.hpp"

#include <tbb/tbb.h>
#include <tbb/enumerable_thread_specific.h>

namespace libfive {

template <typename T, typename Neighbors, unsigned N>
void run(typename WorkerPool<T, Neighbors, N>::Task& task,
         tbb::enumerable_thread_specific<Evaluator>& tlEvs,
         tbb::enumerable_thread_specific<typename T::Pool>& tlPools,
         const BRepSettings& settings,
         int level)
{
    using Task = typename WorkerPool<T, Neighbors, N>::Task;

    auto  eval        = &tlEvs.local();
    auto& object_pool = tlPools.local();
    auto tape = task.tape;
    auto t    = task.target;

    if (settings.cancel.load()) {
        tbb::task::self().cancel_group_execution();
        return;
    }

    // Find our local neighbors.  We do this at the last minute to
    // give other threads the chance to populate more pointers.
    Neighbors neighbors;
    if (t->parent)
    {
        neighbors = task.parent_neighbors.push(
            t->parent_index, t->parent->children);
    }

    // If this tree is larger than the minimum size, then it will either
    // be unambiguously filled/empty, or we'll need to recurse.
    const bool can_subdivide = t->region.level > 0;
    if (can_subdivide)
    {
        Tape::Handle next_tape;
        if (task.vol) {
            auto i = task.vol->check(t->region);
            if (i == Interval::EMPTY || i == Interval::FILLED) {
                t->setType(i);
            }
        }
        if (t->type == Interval::UNKNOWN) {
            next_tape = t->evalInterval(eval, task.tape, object_pool);
        }
        if (next_tape != nullptr) {
            std::swap(tape, next_tape);
        }

        // If this Tree is ambiguous, then push the children to the stack
        // and keep going (because all the useful work will be done
        // by collectChildren eventually).
        assert(t->type != Interval::UNKNOWN);
        if (t->type == Interval::AMBIGUOUS)
        {
            auto rs = t->region.subdivide();
            unsigned sz = unsigned(t->children.size());

            auto recurse = [&](Task task) {
                run<T, Neighbors, N>(task, tlEvs, tlPools, settings, level + 1);
            };

            if (level < 9) {
                tbb::task_group g;
                for (unsigned i = 0u; i < sz; i++) {
                    auto next_tree = object_pool.get(t, i, rs[i]);
                    auto next_vol  = task.vol ? task.vol->push(i, rs[i].perp) : nullptr;
                    g.run(std::bind(recurse, Task {next_tree, tape, neighbors, next_vol}));
                }
                g.wait();

                if (settings.cancel.load()) {
                    tbb::task::self().cancel_group_execution();
                    return;
                }
            }
            else {
                for (unsigned i = 0u; i < sz; i++) {
                    auto next_tree = object_pool.get(t, i, rs[i]);
                    auto next_vol  = task.vol ? task.vol->push(i, rs[i].perp) : nullptr;
                    recurse(Task{next_tree, tape, neighbors, next_vol});
                }
            }
            return;
        }
    }
    else
    {
        t->evalLeaf(eval, tape, object_pool, neighbors);
    }

    if (settings.progress_handler)
    {
        if (can_subdivide)
        {
            // Accumulate all of the child XTree cells that would have been
            // included if we continued to subdivide this tree, then pass
            // all of them to the progress tracker
            uint64_t ticks = 0;
            for (int i=0; i < t->region.level; ++i) {
                ticks = (ticks + 1) * (1 << N);
            }
            settings.progress_handler->tick(ticks + 1);
        }
        else
        {
            settings.progress_handler->tick(1);
        }
    }

    // When all of the children are done, then ask the parent to collect them
    // (recursively, merging the trees on the way up, and reporting
    // completed tree cells to the progress tracker if present).
    auto up = [&]{
        t = t->parent;
        if (t) {
            tape = tape->getBase(t->region.region3());
        }
    };
    up();
    while (t != nullptr && t->collectChildren(eval, tape,
                                              object_pool,
                                              settings.max_err))
    {
        // Report the volume of completed trees as we walk back
        // up towards the root of the tree.
        if (settings.progress_handler) {
            settings.progress_handler->tick();
        }
        up();
    }
}

template<typename T, typename Neighbors, unsigned N>
Root<T> WorkerPool<T, Neighbors, N>::build(Tree                t,
                                           const Region<N>&    region_,
                                           const BRepSettings& settings)
{
    tbb::enumerable_thread_specific<Evaluator> evaluators(t);
    return build(evaluators, region_, settings);
}

template<typename T, typename Neighbors, unsigned N>
Root<T> WorkerPool<T, Neighbors, N>::build(tbb::enumerable_thread_specific<Evaluator>& evaluators,
  const Region<N>& region_,
  const BRepSettings& settings)
{
    tbb::enumerable_thread_specific<typename T::Pool> tlPools;
    
    const auto region = region_.withResolution(settings.min_feature);
    auto       root(new T(nullptr, 0, region));
    
    if (settings.progress_handler) {
        // Kick off the progress tracking thread, based on the number of
        // octree levels and a fixed split per level
        uint64_t ticks = 0;
        for (int i=0; i < region.level; ++i) {
            ticks = (ticks + 1) * (1 << N);
        }
        settings.progress_handler->nextPhase(ticks + 1);
    }
    
    Evaluator& ev = evaluators.local();
    Task task({root, ev.getDeck()->tape, Neighbors(), settings.vol});
    
    run<T, Neighbors, N>(task, evaluators, tlPools, settings, 0);
    
    if (settings.cancel.load())
    {
        return Root<T>();
    }
    
    Root<T> out(root);
    for (auto& pool : tlPools) {
        out.claim(pool);
    }
    
    return out;
}

}   // namespace libfive
