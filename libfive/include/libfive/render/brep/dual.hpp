/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <stack>
#include <boost/lockfree/stack.hpp>

#include "libfive/render/brep/per_thread_brep.hpp"
#include "libfive/render/brep/free_thread_handler.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/root.hpp"

#include "libfive/render/axes.hpp"
#include "libfive/eval/interval.hpp"

namespace libfive {

/*
 *  Class to walk a dual grid for a quad or octree
 */
template <unsigned N>
class Dual
{
public:
     /*
      *  Basic dual-walking function
      *
      *  The mesher type M needs
      *     load(const std::array<T*, N>& trees)
      *     Input (typename)
      *     Output (typename)
      *  and must have a constructor of the form
      *     M(PerThreadBRep<N>&, A...)
      */
    template<typename M, typename ... A>
    static std::unique_ptr<typename M::Output> walk(
            const Root<typename M::Input>& t,
            const BRepSettings& settings,
            A... args);

     /*
      *  Flexible dual-walking function
      *
      *  The mesher type M needs
      *     load(const std::array<T*, N>& trees)
      *     Input (typename)
      *     Output (typename)
      *
      *  The factory can be anything that spits out valid M objects,
      *  given a PerThreadBRep and worker index.
      */
    template<typename M>
    static std::unique_ptr<typename M::Output> walk_(
            const Root<typename M::Input>& t,
            const BRepSettings& settings,
            std::function<M(PerThreadBRep<N>&, int)> MesherFactory);

protected:
    template<typename T, typename Mesher>
    static void run(Mesher& m,
                    boost::lockfree::stack<const T*,
                                           boost::lockfree::fixed_sized<true>>& tasks,
                    const BRepSettings& settings,
                    std::atomic_bool& done);

    template <typename T, typename Mesher>
    static void work(const T* t, Mesher& m);

    template <typename T, typename Mesher>
    static void handleTopEdges(T* t, Mesher& m);
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

template <>
template <typename T, typename Mesher>
void Dual<2>::handleTopEdges(T* t, Mesher& m)
{
    (void)t;
    (void)m;

    // TODO
    // No one should be calling this yet, because simplex meshing
    // isn't implemented in 2D.
    assert(false);
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

template <>
template <typename T, typename V>
void Dual<3>::handleTopEdges(T* t, V& v)
{
    auto e = T::empty();

    for (unsigned i=0; i < 4; ++i)
    {
        std::array<T*, 4> ts = {{e.get(), e.get(), e.get(), e.get()}};
        ts[i] = t;
        edge3<T, V, Axis::X>(ts, v);
        edge3<T, V, Axis::Y>(ts, v);
        edge3<T, V, Axis::Z>(ts, v);
    }

    for (unsigned i=0; i < 2; ++i)
    {
        std::array<T*, 2> ts = {{e.get(), e.get()}};
        ts[i] = t;
        face3<T, V, Axis::X>(ts, v);
        face3<T, V, Axis::Y>(ts, v);
        face3<T, V, Axis::Z>(ts, v);
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
template<typename M, typename ... A>
std::unique_ptr<typename M::Output> Dual<N>::walk(
            const Root<typename M::Input>& t,
            const BRepSettings& settings,
            A... args)
{
    return walk_<M>(
            t, settings,
            [&args...](PerThreadBRep<N>& brep, int i) {
                (void)i;
                return M(brep, args...);
                });

}

template <unsigned N>
template<typename M>
std::unique_ptr<typename M::Output> Dual<N>::walk_(
            const Root<typename M::Input>& t,
            const BRepSettings& settings,
            std::function<M(PerThreadBRep<N>&, int)> MesherFactory)
{
    boost::lockfree::stack<
        const typename M::Input*,
        boost::lockfree::fixed_sized<true>> tasks(settings.workers);
    tasks.push(t.get());
    t->resetPending();

    std::atomic<uint32_t> global_index(1);
    std::vector<PerThreadBRep<N>> breps;
    for (unsigned i=0; i < settings.workers; ++i) {
        breps.emplace_back(PerThreadBRep<N>(global_index));
    }

    if (settings.progress_handler) {
        settings.progress_handler->nextPhase(t.size() + 1);
    }

    std::vector<std::future<void>> futures;
    futures.resize(settings.workers);
    std::atomic_bool done(false);
    for (unsigned i=0; i < settings.workers; ++i) {
        futures[i] = std::async(std::launch::async,
            [&breps, &tasks, &MesherFactory, &settings, &done, i]()
            {
                auto m = MesherFactory(breps[i], i);
                Dual<N>::run(m, tasks, settings, done);
            });
    }

    // Wait on all of the futures
    for (auto& f : futures) {
        f.get();
    }

    assert(done.load() || settings.cancel.load());

    // Handle the top tree edges (only used for simplex meshing)
    if (M::needsTopEdges()) {
        auto m = MesherFactory(breps[0], 0);
        Dual<N>::handleTopEdges(t.get(), m);
    }

    auto out = std::unique_ptr<typename M::Output>(new typename M::Output);
    out->collect(breps);
    return out;
}


template <unsigned N>
template <typename T, typename V>
void Dual<N>::run(V& v,
                  boost::lockfree::stack<const T*,
                                         boost::lockfree::fixed_sized<true>>& tasks,
                  const BRepSettings& settings,
                  std::atomic_bool& done)

{
    // Tasks to be evaluated by this thread (populated when the
    // MPMC stack is completely full).
    std::stack<const T*, std::vector<const T*>> local;

    while (!done.load() && !settings.cancel.load())
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
            if (settings.free_thread_handler != nullptr) {
                settings.free_thread_handler->offerWait();
            }
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

        // Special-case for singleton trees, which have null parents
        // (and have already been subtracted from pending)
        if (T::isSingleton(t)) {
            continue;
        }

        if (settings.progress_handler) {
            settings.progress_handler->tick();
        }

        for (t = t->parent; t && t->pending-- == 0; t = t->parent)
        {
            // Do the actual DC work (specialized for N = 2 or 3)
            Dual<N>::work(t, v);

            // Report trees as completed
            if (settings.progress_handler) {
                settings.progress_handler->tick();
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

}   // namespace libfive
