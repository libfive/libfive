/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <stack>
#include "libfive/render/brep/per_thread_brep.hpp"
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/render/brep/mesh.hpp"

namespace Kernel {

/*
 *  Class to walk a dual grid for a quad or octree
 */
template <unsigned N>
class Dual
{
public:
     /*  Mesher needs load(const std::array<T*, N>& trees) defined
      *
      *  Factory needs operator() defined, and should produce a mesher
      *  object when passed a PerThreadBRep<N> */
    template<typename Output, typename T, typename F>
    static Output walk(const T* tree, F& factory, unsigned workers=1,
                       ProgressWatcher* p=nullptr);

protected:
    template<typename T, typename Mesher>
    static void run(Mesher& m, ProgressWatcher* progress,
                    boost::lockfree::stack<const T*,
                                           boost::lockfree::fixed_sized<true>>& tasks,
                    std::atomic_bool& done, std::atomic_bool& cancel);

    template <typename T, typename Mesher>
    static void work(const T* t, Mesher& m);
};

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <typename T, typename V, Axis::Axis A>
void edge2(const std::array<const T*, 2>& ts, V& v)
{
    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;

    if (std::any_of(ts.begin(), ts.end(),
        [](const T* t){ return t->isBranch(); }))
    {
        edge2<T, V, A>({{ts[0]->child(perp), ts[1]->child(0)}}, v);
        edge2<T, V, A>({{ts[0]->child(A|perp), ts[1]->child(A)}}, v);
    }
    else if (std::all_of(ts.begin(), ts.end(),
        [](const T* t){ return t->type == Interval::AMBIGUOUS &&
                               !t->isBranch(); }))
    {
        v.template load<A>(ts);
    }
}

template <>
template <typename T, typename V>
void Dual<2>::work(const T* t, V& v)
{
    edge2<T, V, Axis::Y>({{t->child(0), t->child(Axis::X)}}, v);
    edge2<T, V, Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, v);
    edge2<T, V, Axis::X>({{t->child(0), t->child(Axis::Y)}}, v);
    edge2<T, V, Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, v);
}

////////////////////////////////////////////////////////////////////////////////
// 3D Implementation
template <typename T, typename V, Axis::Axis A>
void edge3(const std::array<const T*, 4> ts, V& v)
{
    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    if (std::any_of(ts.begin(), ts.end(),
        [](const T* t){ return t->isBranch(); }))
    {
        edge3<T, V, A>({{ts[0]->child(Q|R), ts[1]->child(R), ts[2]->child(Q), ts[3]->child(0)}}, v);
        edge3<T, V, A>({{ts[0]->child(Q|R|A), ts[1]->child(R|A), ts[2]->child(Q|A), ts[3]->child(A)}}, v);
    }
    else
    {
        v.template load<A>(ts);
    }
}

template <typename T, typename V, Axis::Axis A>
void face3(const std::array<const T*, 2> ts, V& v)
{
    if (std::any_of(ts.begin(), ts.end(),
        [](const T* t){ return t->isBranch(); }))
    {
        constexpr auto Q = Axis::Q(A);
        constexpr auto R = Axis::R(A);

        for (unsigned k : {0, (int)Q, (int)R, Q|R})
        {
            face3<T, V, A>({{ts[0]->child(k|A), ts[1]->child(k)}}, v);
        }

        edge3<T, V, Q>({{ts[0]->child(A), ts[0]->child(R|A), ts[1]->child(0), ts[1]->child(R)}}, v);
        edge3<T, V, Q>({{ts[0]->child(Q|A), ts[0]->child(Q|R|A), ts[1]->child(Q), ts[1]->child(Q|R)}}, v);

        edge3<T, V, R>({{ts[0]->child(A), ts[1]->child(0), ts[0]->child(A|Q), ts[1]->child(Q)}}, v);
        edge3<T, V, R>({{ts[0]->child(R|A), ts[1]->child(R), ts[0]->child(R|A|Q), ts[1]->child(R|Q)}}, v);
    }
}

template <typename T, typename V, Axis::Axis A>
void call_edge3(const T* t, V& v)
{
    for (auto a : {Axis::Axis(0), A})
    {
        edge3<T, V, A>({{t->child(a),
             t->child(Axis::Q(A) | a),
             t->child(Axis::R(A) | a),
             t->child(Axis::Q(A) | Axis::R(A) | a)}}, v);
    }
}

template <typename T, typename V, Axis::Axis A>
void call_face3(const T* t, V& v)
{
    constexpr auto q = Axis::Q(A);
    constexpr auto r = Axis::R(A);

    face3<T, V, A>({{t->child(0), t->child(A)}}, v);
    face3<T, V, A>({{t->child(q), t->child(q|A)}}, v);
    face3<T, V, A>({{t->child(r), t->child(r|A)}}, v);
    face3<T, V, A>({{t->child(q|r), t->child(q|r|A)}}, v);
}

template <>
template <typename T, typename V>
void Dual<3>::work(const T* t, V& v)
{
    // Call the face procedure on every pair of cells (4x per axis)
    call_face3<T, V, Axis::X>(t, v);
    call_face3<T, V, Axis::Y>(t, v);
    call_face3<T, V, Axis::Z>(t, v);

    // Call the edge function 6 times (2x per axis)
    call_edge3<T, V, Axis::X>(t, v);
    call_edge3<T, V, Axis::Y>(t, v);
    call_edge3<T, V, Axis::Z>(t, v);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
template <typename Output, typename T, typename Factory>
Output Dual<N>::walk(const T* t, Factory& f, unsigned workers,
                     ProgressWatcher* progress)
{
    boost::lockfree::stack<const T*, boost::lockfree::fixed_sized<true>>
        tasks(workers);
    tasks.push(t);

    std::atomic_uint32_t global_index(1);
    std::vector<PerThreadBRep<N>> breps;
    for (unsigned i=0; i < workers; ++i) {
        breps.emplace_back(PerThreadBRep<N>(global_index));
    }

    std::atomic_bool done(false);
    std::atomic_bool cancel(false); // TODO: connect this from above

    std::vector<std::future<void>> futures;
    futures.resize(workers);
    for (unsigned i=0; i < workers; ++i) {
        futures[i] = std::async(std::launch::async,
            [&breps, &done, &cancel, &tasks, &f, i, progress]() {
                auto m = f(breps[i]);
                Dual<N>::run(m, progress, tasks, done, cancel);
            });
    }

    // Wait on all of the futures
    for (auto& f : futures) {
        f.get();
    }

    assert(done.load() || cancel.load());

    Output out;
    out.collect(breps);
    return out;
}


template <unsigned N>
template <typename T, typename V>
void Dual<N>::run(V& v, ProgressWatcher* progress,
                  boost::lockfree::stack<const T*,
                                         boost::lockfree::fixed_sized<true>>& tasks,
                  std::atomic_bool& done, std::atomic_bool& cancel)
{
    // Tasks to be evaluated by this thread (populated when the
    // MPMC stack is completely full).
    std::stack<const T*, std::vector<const T*>> local;

    while (!done.load() && !cancel.load())
    {
        // Prioritize picking up a local task before going to
        // the MPMC queue, to keep things in this thread for
        // as long as possible.
        const T* t;
        if (local.size())
        {
            t = local.top();
            local.pop();
        }
        else if (!tasks.pop(t))
        {
            t = nullptr;
        }

        // If we failed to get a task, keep looping
        // (so that we terminate when either of the flags are set).
        if (t == nullptr)
        {
            continue;
        }

        if (t->isBranch())
        {
            // Recurse, calling the cell procedure for every child
            for (const auto& c_ : t->children)
            {
                const auto c = c_.load();
                if (!tasks.bounded_push(c)) {
                    local.push(c);
                }
            }
            continue;
        }

        for (t = t->parent;
             t && t->pending.fetch_add(1) == t->children.size() - 2;
             t = t->parent)
        {
            // Do the actual DC work (specialized for N = 2 or 3)
            Dual<N>::work(t, v);

            // Report trees as completed
            if (progress) {
                progress->tick();
            }
        }

        // Termination condition:  if we've ended up pointing at the parent
        // of the tree's root (which is nullptr), then we're done and break
        if (t == nullptr) {
            break;
        }
    }

    // If we've broken out of the loop, then we should set the done flag
    // so that other worker threads also terminate.
    done.store(true);
}

}   // namespace Kernel
