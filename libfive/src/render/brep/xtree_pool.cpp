/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <future>
#include <numeric>
#include <functional>
#include <limits>
#include <stack>

#include <cmath>

#include <boost/lockfree/stack.hpp>

#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/xtree_pool.hpp"
#include "libfive/render/brep/pool.hpp"
#include "libfive/eval/tape.hpp"

namespace Kernel {

template <unsigned N>
using LockFreeStack =
    boost::lockfree::stack<Task<N>, boost::lockfree::fixed_sized<true>>;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
static void run(
        XTreeEvaluator* eval, LockFreeStack<N>& tasks,
        const float min_feature, const float max_err,
        std::atomic_bool& done, std::atomic_bool& cancel,
        typename XTree<N>::Root& root, std::mutex& root_lock,
        boost::lockfree::stack<float>* progress)
{
    // Tasks to be evaluated by this thread (populated when the
    // MPMC stack is completely full).
    std::stack<Task<N>, std::vector<Task<N>>> local;

    Pool<XTree<N>> spare_trees;
    Pool<typename XTree<N>::Leaf> spare_leafs;

    while (!done.load() && !cancel.load())
    {
        // Prioritize picking up a local task before going to
        // the MPMC queue, to keep things in this thread for
        // as long as possible.
        Task<N> task;
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
        const auto& region = task.region;

        // Find our local neighbors.  We do this at the last minute to
        // give other threads the chance to populate more pointers.
        Neighbors<N> neighbors;
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
                    Task<N> next(spare_trees.get(t, i), tape, rs[i], neighbors);
                    if (!tasks.bounded_push(next))
                    {
                        local.push(next);
                    }
                }

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
                // TODO: report progress based on skipping all children
            }
            else
            {
                progress->push(region.volume());
            }
        }

        // If all of the children are done, then ask the parent to collect them
        // (recursively, merging the trees on the way up, and reporting volumes
        // to the progress tracker if present).
        double volume = region.volume();
        for (t = t->parent;
             t && t->collectChildren(eval, tape, max_err, region.perp,
                                     spare_trees, spare_leafs);
             t = t->parent)
        {
            // Report the volume of completed trees as we walk back
            // up towards the root of the tree.
            if (progress)
            {
                volume *= (1 << N);
                progress->push(volume);
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

template <unsigned N>
typename XTree<N>::Root XTreePool<N>::build(
            const Tree t, Region<N> region,
            double min_feature, double max_err, unsigned workers,
            std::function<bool(float)> progress_callback)
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
typename XTree<N>::Root XTreePool<N>::build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            std::function<bool(float)> progress_callback)
{
    // Lazy initialization of marching squares / cubes table
    if (XTree<N>::mt.get() == nullptr)
    {
        XTree<N>::mt = Marching::buildTable<N>();
    }

    auto root(new XTree<N>(nullptr, 0));
    std::atomic_bool done(false);

    LockFreeStack<N> tasks(workers);
    tasks.push(Task<N>(root, eval->deck->tape, region, Neighbors<N>()));

    std::vector<std::future<void>> futures;
    futures.resize(workers);

    typename XTree<N>::Root out(root);
    std::mutex root_lock;

    const bool has_progress_callback =
        *progress_callback.target<bool(*)(float)>() != EMPTY_PROGRESS_CALLBACK;
    boost::lockfree::stack<float> progress;
    auto progress_ptr = has_progress_callback ? &progress : nullptr;

    std::future<void> progress_task;
    if (has_progress_callback)
    {
        progress_task = std::async(std::launch::async,
            [&progress, &progress_callback, &done, &cancel]()
            {
                while (!done.load() && !cancel.load())
                {
                    float next;
                    progress.pop(next);
                    std::cout << next << "\n";
                }
                if (!cancel.load())
                {
                    progress_callback(1.0f);
                }
            }
        );
    }

    for (unsigned i=0; i < workers; ++i)
    {
        futures[i] = std::async(std::launch::async,
                [&eval, &tasks, &cancel, &done, &out, &root_lock,
                 min_feature, max_err, i, progress_ptr](){
                    run(eval + i, tasks, min_feature, max_err,
                        done, cancel, out, root_lock, progress_ptr);
                    });
    }

    // Wait on all of the futures
    for (auto& f : futures)
    {
        f.get();
    }

    assert(done.load() || cancel.load());

    // Wait for the progress bar to finish, pushing one value to it
    // (which forces it to test the done and cancel conditions again)
    if (has_progress_callback)
    {
        progress.push(0);
        progress_task.wait();
    }

    if (cancel.load())
    {
        return typename XTree<N>::Root();
    }
    else
    {
        return out;
    }
}

}   // namespace Kernel
