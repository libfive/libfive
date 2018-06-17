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
        std::atomic_bool& done, std::atomic_bool& cancel)
{
    std::stack<Task<N>, std::vector<Task<N>>> local;
    Pool<XTree<N>> spare_trees;

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
        if (((region.upper - region.lower) > min_feature).any())
        {
            tape = t->evalInterval(eval->interval, region, task.tape);

            // If this Tree is ambiguous, then push the children to the stack
            // and keep going (because all the useful work will be done
            // by collectChildren eventually).
            if (t->type == Interval::AMBIGUOUS || t->type == Interval::UNKNOWN)
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
            t->evalLeaf(eval, neighbors, region, tape);
        }

        // If all of the children are done, then ask the parent to collect them
        // (recursively, merging the trees on the way up)
        for (t = t->parent;
             t && t->collectChildren(eval, tape, max_err, region.perp, spare_trees);
             t = t->parent);

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
}

template <unsigned N>
std::unique_ptr<XTree<N>> XTreePool<N>::build(
            const Tree t, Region<N> region,
            double min_feature, double max_err,
            unsigned workers)
{
    std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
    es.reserve(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        es.emplace_back(XTreeEvaluator(t));
    }
    std::atomic_bool cancel(false);
    return XTreePool<N>::build(es.data(), region, min_feature,
                               max_err, workers, cancel);
}

template <unsigned N>
std::unique_ptr<XTree<N>> XTreePool<N>::build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel)
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
    for (unsigned i=0; i < workers; ++i)
    {
        futures[i] = std::async(std::launch::async,
                [&eval, &tasks, &cancel, &done,
                 min_feature, max_err, i](){
                    run(eval + i, tasks, min_feature, max_err,
                        done, cancel);
                    });
    }

    // Wait on all of the futures
    for (auto& f : futures)
    {
        f.get();
    }

    if (cancel.load())
    {
        delete root;
        root = nullptr;
    }
    return std::unique_ptr<XTree<N>>(root);
}

}   // namespace Kernel
