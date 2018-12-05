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
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK)
    {
        std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
        es.reserve(workers);
        for (unsigned i=0; i < workers; ++i)
        {
            es.emplace_back(XTreeEvaluator(t));
        }
        std::atomic_bool cancel(false);
        return build(es.data(), region, min_feature,
                     max_err, workers, cancel, progress_callback);
    }

    /*
     *  Full-featured builder function
     */
    static Root<T> build(XTreeEvaluator* eval,
            Region<N> region, double min_feature,
            double max_err, unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK)
    {
        auto root(new T(nullptr, 0, region));
        std::atomic_bool done(false);

        LockFreeStack tasks(workers);
        tasks.push({root, eval->deck->tape, region, Neighbors()});

        std::vector<std::future<void>> futures;
        futures.resize(workers);

        Root<T> out(root);
        std::mutex root_lock;

        // Kick off the progress tracking thread, based on the number of
        // octree levels and a fixed split per level
        auto min_dimension = (region.upper - region.lower).minCoeff();
        const unsigned levels = ceil(log(min_dimension / min_feature) / log(2));
        uint64_t ticks = 0;
        for (unsigned i=0; i <= levels; ++i)
        {
            ticks = (ticks + 1) * (1 << N);
        }
        auto progress_watcher = ProgressWatcher::build(ticks, 0, progress_callback,
                                                       done, cancel);

        for (unsigned i=0; i < workers; ++i)
        {
            futures[i] = std::async(std::launch::async,
                    [&eval, &tasks, &cancel, &done, &out, &root_lock,
                     min_feature, max_err, i, progress_watcher](){
                        run(eval + i, tasks, min_feature, max_err,
                            done, cancel, out, root_lock, progress_watcher);
                        });
        }

        // Wait on all of the futures
        for (auto& f : futures)
        {
            f.get();
        }

        assert(done.load() || cancel.load());

        // Wait for the progress bar to finish, which happens in the destructor.
        delete progress_watcher;

        if (cancel.load())
        {
            return Root<T>();
        }
        else
        {
            return out;
        }
    }

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
                    ProgressWatcher* progress)
    {
        // Tasks to be evaluated by this thread (populated when the
        // MPMC stack is completely full).
        std::stack<Task, std::vector<Task>> local;

        ObjectPool<T> spare_trees;
        ObjectPool<typename T::Leaf> spare_leafs;

        while (!done.load() && !cancel.load())
        {
            // Prioritize picking up a local task before going to
            // the MPMC queue, to keep things in this thread for
            // as long as possible.
            Task task;
            if (local.size())
            {
                task = local.top();
                local.pop();
            }
            else if (!tasks.pop(task))
            {
                task.target = nullptr;
            }

            // If we failed to get a task, keep looping
            // (so that we terminate when either of the flags are set).
            if (task.target == nullptr)
            {
                continue;
            }

            auto tape = task.tape;
            auto t = task.target;
            Region<N> region = task.region;

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
            const double min_dimension = (region.upper - region.lower).minCoeff();
            const bool can_subdivide =  min_dimension > min_feature;
            if (can_subdivide)
            {
                tape = t->evalInterval(eval->interval, region, task.tape);

                // If this Tree is ambiguous, then push the children to the stack
                // and keep going (because all the useful work will be done
                // by collectChildren eventually).
                assert(t->type != Interval::UNKNOWN);
                if (t->type == Interval::AMBIGUOUS)
                {
                    auto rs = region.subdivide();
                    for (unsigned i=0; i < t->children.size(); ++i)
                    {
                        // If there are available slots, then pass this work
                        // to the queue; otherwise, undo the decrement and
                        // assign it to be evaluated locally.
                        T* next_tree;
                        spare_trees.get(&next_tree, t, i, rs[i]);
                        Task next{next_tree, tape, rs[i], neighbors};
                        if (!tasks.bounded_push(next))
                        {
                            local.push(next);
                        }
                    }

                    // If we did an interval evaluation, then we either
                    // (a) are done with this tree because it is empty / filled
                    // (b) don't do anything until all of its children are done
                    //
                    // In both cases, we should keep looping; the latter case
                    // is handled in collectChildren below.
                    continue;
                }
            }
            else
            {
                t->evalLeaf(eval, neighbors, region, tape, spare_leafs);
            }

            if (progress)
            {
                if (can_subdivide)
                {
                    // Accumulate all of the child XTree cells that would have been
                    // included if we continued to subdivide this tree, then pass
                    // all of them to the progress tracker
                    const unsigned levels =
                        ceil(log(min_dimension / min_feature) / log(2));
                    uint64_t ticks = 0;
                    for (unsigned i=0; i <= levels; ++i)
                    {
                        ticks = (ticks + 1) * (1 << N);
                    }
                    progress->tick(ticks);
                }
                else
                {
                    progress->tick(1);
                }
            }

            // If all of the children are done, then ask the parent to collect them
            // (recursively, merging the trees on the way up, and reporting
            // completed tree cells to the progress tracker if present).
            for (region = region.parent(t->parent_index), t = t->parent;
                 t && t->collectChildren(eval, tape, max_err, region,
                                         spare_trees, spare_leafs);
                 region = region.parent(t->parent_index), t = t->parent)
            {
                // Report the volume of completed trees as we walk back
                // up towards the root of the tree.
                if (progress)
                {
                    progress->tick();
                }
            }

            // Termination condition:  if we've ended up pointing at the parent
            // of the tree's root (which is nullptr), then we're done and break
            if (t == nullptr)
            {
                break;
            }
        }

        // If we've broken out of the loop, then we should set the done flag
        // so that other worker threads also terminate.
        done.store(true);

        {   // Release the pooled objects to the root
            std::lock_guard<std::mutex> lock(root_lock);
            root.claim(spare_leafs);
            root.claim(spare_trees);
        }
    }
};

}   // namespace Kernel
