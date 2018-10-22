/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/dc/xtree.hpp"
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

/*
 *  Class to walk a dual grid for a quad or octree
 *  t needs operator(const std::array<T*, N>& trees) defined
 */
template <unsigned N>
class Dual
{
public:
    template<typename T, typename V>
    static void walk(const T* tree, V& v, ProgressWatcher* p=nullptr);
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
void Dual<2>::walk(const T* t, V& v, ProgressWatcher* progress)
{
    if (t->isBranch())
    {
        // Recurse down every subface in the quadtree
        for (unsigned i=0; i < t->children.size(); ++i)
        {
            auto c = t->child(i);
            if (c != t)
            {
                walk(c, v, progress);
            }
        }

        //  Then, call edge on every pair of cells
        edge2<T, V, Axis::Y>({{t->child(0), t->child(Axis::X)}}, v);
        edge2<T, V, Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, v);
        edge2<T, V, Axis::X>({{t->child(0), t->child(Axis::Y)}}, v);
        edge2<T, V, Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, v);
    }

    if (progress != nullptr)
    {
        progress->tick();
    }
}
////////////////////////////////////////////////////////////////////////////////

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
void Dual<3>::walk(const T* t, V& v, ProgressWatcher* progress)
{
    if (t->isBranch())
    {
        // Recurse, calling the cell procedure for every child
        for (unsigned i=0; i < t->children.size(); ++i)
        {
            auto c = t->child(i);
            if (c != t)
            {
                walk(c, v, progress);
            }
        }

        // Call the face procedure on every pair of cells (4x per axis)
        call_face3<T, V, Axis::X>(t, v);
        call_face3<T, V, Axis::Y>(t, v);
        call_face3<T, V, Axis::Z>(t, v);

        // Call the edge function 6 times (2x per axis)
        call_edge3<T, V, Axis::X>(t, v);
        call_edge3<T, V, Axis::Y>(t, v);
        call_edge3<T, V, Axis::Z>(t, v);
    }
    if (progress != nullptr)
    {
        progress->tick();
    }
}

}   // namespace Kernel
