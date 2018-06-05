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

#include <cmath>


#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/xtree_pool.hpp"
#include "libfive/eval/tape.hpp"

namespace Kernel {

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
void XTreePool<N>::run(
        XTreeEvaluator* eval, boost::lockfree::queue<Task<N>*>& tasks,
        const float min_feature, const float max_err,
        std::atomic_bool& done, std::atomic_bool& cancel)
{
    while (!done.load() && !cancel.load())
    {
        std::unique_ptr<Task<N>> task;
        {   // Store the task in an RAII handle
            Task<N>* task_;
            if (!tasks.pop(task_))
            {
                continue;
            }
            task.reset(task_);
        }

        auto tape = task->tape;
        auto t = task->target;

        if (((t->region.upper - t->region.lower) > min_feature).any())
        {
            tape = t->evalInterval(eval->interval, task->tape);

            // If this Tree is ambiguous, then push the children to the queue
            // and keep going (because all the useful work will be done
            // by collectChildren eventually).
            if (t->type == Interval::AMBIGUOUS || t->type == Interval::UNKNOWN)
            {
                auto rs = t->region.subdivide();
                for (unsigned i=0; i < t->children.size(); ++i)
                {
                    auto next = new Task<N>();
                    t->children[i].store(new XTree<N>(t, rs[i]));
                    next->target = t->children[i];
                    next->tape = tape;

                    tasks.push(next);
                }
                continue;
            }
            // First termination condition: if the root of the XTree is
            // empty or filled, then return right away.
            else if (t->parent == nullptr)
            {
                done.store(true);
            }
        }
        else
        {
            t->evalLeaf(eval, tape);
        }

        // If all of the children are done, then ask the parent to collect them
        // (recursively, merging the trees on the way up)
        auto target = t->parent;
        while(target && target->collectChildren(eval, tape, max_err))
        {
            target = target->parent;
            // The second termination condition: if we successfully call
            // collectChildren on the root of the XTree, then we're done.
            if (target == nullptr)
            {
                done.store(true);
            }
        }
    }
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
    std::atomic<XTree<N>*> root(new XTree<N>(nullptr, region));
    std::atomic_bool done(false);

    boost::lockfree::queue<Task<N>*> tasks;
    auto task = new Task<N>;
    task->target = root;
    task->tape = eval->deck->tape;

    tasks.push(task);

    std::vector<std::future<void>> futures;
    futures.resize(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        futures[i] = std::async(std::launch::async,
                [&eval, &tasks, &cancel, &done, min_feature, max_err, i](){
                    run(eval + i, tasks, min_feature, max_err,
                        done, cancel);
                    });
    }

    // Wait on all of the futures
    for (auto& f : futures)
    {
        f.get();
    }

    return std::unique_ptr<XTree<N>>(root.load());
}

}   // namespace Kernel
