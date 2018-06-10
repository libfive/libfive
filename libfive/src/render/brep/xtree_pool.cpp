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


#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/xtree_pool.hpp"
#include "libfive/eval/tape.hpp"

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void XTreePool<N>::run(
        XTreeEvaluator* eval, boost::lockfree::queue<Task<N>*>& tasks,
        const float min_feature, const float max_err, std::atomic_int& slots,
        std::atomic_bool& done, std::atomic_bool& cancel)
{
    std::unique_ptr<Task<N>> task;
    bool idle = false;
    std::stack<Task<N>*, std::vector<Task<N>*>> local;

    while (!done.load() && !cancel.load())
    {
        {   // Prioritize picking up a local task before going to
            // the MPMC queue, to keep things in this thread for
            // as long as possible.
            Task<N>* task_;
            if (local.size())
            {
                task_ = local.top();
                local.pop();
            }
            else if (!tasks.pop(task_))
            {
                task_ = nullptr;
            }
            task.reset(task_);
        }

        // If we failed to get a task, then mark this thread as idle
        // and keep looping (so that we terminate when either of the
        // flags are set).
        if (task.get() == nullptr)
        {
            if (!idle)
            {
                slots++;
                idle = true;
            }
            continue;
        }
        // Otherwise, mark that this thread is no longer available
        else if (idle)
        {
            idle = false;
            slots--;
        }

        auto tape = task->tape;
        auto t = task->target;

        if (((t->region.upper - t->region.lower) > min_feature).any())
        {
            tape = t->evalInterval(eval->interval, task->tape);

            // If this Tree is ambiguous, then push the children to the stack
            // and keep going (because all the useful work will be done
            // by collectChildren eventually).
            if (t->type == Interval::AMBIGUOUS || t->type == Interval::UNKNOWN)
            {
                auto rs = t->region.subdivide();
                for (unsigned i=0; i < t->children.size(); ++i)
                {
                    auto next = new Task<N>();
                    auto target = new XTree<N>(t, i, rs[i]);
                    next->target = target;
                    next->tape = tape;

                    // If there are available slots, then pass this work
                    // to the queue; otherwise, undo the decrement and
                    // assign it to be evaluated locally.
                    if (slots.load() > 0)
                    {
                        tasks.push(next);
                    }
                    else
                    {
                        local.push(next);
                    }
                }

                continue;
            }
            // First termination condition: if the root of the XTree is
            // empty or filled, then return right away.
            else if (t->parent == nullptr)
            {
                break;
            }
        }
        else
        {
            t->evalLeaf(eval, tape);

            // Second termination condition: if we did a leaf evaluation
            // on the root of the XTree, then we've been passed a large
            // min_feature and this is the end.
            if (t->parent == nullptr)
            {
                break;
            }
        }

        // If all of the children are done, then ask the parent to collect them
        // (recursively, merging the trees on the way up)
        for (t = t->parent; t && t->collectChildren(eval, tape, max_err);
             t = t->parent);

        // Third termination condition:  If we just collected children at the
        // root of the tree (then moved to point at its parent, which is
        // nullptr), then we're done.
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
std::unique_ptr<const XTree<N>> XTreePool<N>::build(
            const Tree t, Region<N> region,
            double min_feature, double max_err)
{
    auto eval = XTreeEvaluator(t);
    std::atomic_bool cancel(false);
    return XTreePool<N>::build(&eval, region, min_feature, max_err, 1, cancel);
}

template <unsigned N>
std::unique_ptr<const XTree<N>> XTreePool<N>::build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel)
{
    // Lazy initialization of marching squares / cubes table
    if (XTree<N>::mt.get() == nullptr)
    {
        XTree<N>::mt = Marching::buildTable<N>();
    }

    auto root(new XTree<N>(nullptr, 0, region));
    std::atomic_bool done(false);

    boost::lockfree::queue<Task<N>*> tasks(workers * 2);
    auto task = new Task<N>;
    task->target = root;
    task->tape = eval->deck->tape;

    tasks.push(task);
    std::atomic_int slots(0);

    std::vector<std::future<void>> futures;
    futures.resize(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        futures[i] = std::async(std::launch::async,
                [&eval, &tasks, &slots, &cancel, &done,
                 min_feature, max_err, i](){
                    run(eval + i, tasks, min_feature, max_err,
                        slots, done, cancel);
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
