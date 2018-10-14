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
 *  t needs operator(const std::array<XTree<N>*, N>& trees) defined
 */
template <unsigned N>
class Dual
{
public:
    template<typename V>
    static void walk(const XTree<N>* tree, V& v, ProgressWatcher* p=nullptr);
};

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <typename V, Axis::Axis A>
void edge2(const std::array<const XTree<2>*, 2>& ts, V& v)
{
    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;

    if (std::any_of(ts.begin(), ts.end(),
        [](const XTree<2>* t){ return t->isBranch(); }))
    {
        edge2<V, A>({{ts[0]->child(perp), ts[1]->child(0)}}, v);
        edge2<V, A>({{ts[0]->child(A|perp), ts[1]->child(A)}}, v);
    }
    else if (std::all_of(ts.begin(), ts.end(),
        [](const XTree<2>* t){ return t->type == Interval::AMBIGUOUS &&
                                      !t->isBranch(); }))
    {
        // Sanity-checking that all cells have a Leaf struct allocated
        for (auto& t : ts)
        {
            assert(t->leaf != nullptr);
            (void)t;
        }

        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const XTree<2>* a, const XTree<2>* b)
                { return a->leaf->level < b->leaf->level; }) - ts.begin();

        constexpr std::array<uint8_t, 2> corners = {{perp, 0}};

        // If there is a sign change across the relevant edge, then call the
        // watcher with the segment corners (with proper winding order)
        auto a = ts[index]->cornerState(corners[index]);
        auto b = ts[index]->cornerState(corners[index] | A);
        if (a != b)
        {
            // Use either forward or reversed segment building
            if ((a == Interval::FILLED && A == Axis::Y) ||
                (b == Interval::FILLED && A == Axis::X))
            {
                v.template load<A, 0>(ts);
            }
            else
            {
                v.template load<A, 1>(ts);
            }
        }
    }
}

template <>
template <typename V>
void Dual<2>::walk(const XTree<2>* t, V& v, ProgressWatcher* progress)
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
        edge2<V, Axis::Y>({{t->child(0), t->child(Axis::X)}}, v);
        edge2<V, Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, v);
        edge2<V, Axis::X>({{t->child(0), t->child(Axis::Y)}}, v);
        edge2<V, Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, v);
    }

    if (progress != nullptr)
    {
        progress->tick();
    }
}
////////////////////////////////////////////////////////////////////////////////

template <typename V, Axis::Axis A>
void edge3(const std::array<const XTree<3>*, 4> ts, V& v)
{
    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    if (std::any_of(ts.begin(), ts.end(),
        [](const XTree<3>* t){ return t->isBranch(); }))
    {
        edge3<V, A>({{ts[0]->child(Q|R), ts[1]->child(R), ts[2]->child(Q), ts[3]->child(0)}}, v);
        edge3<V, A>({{ts[0]->child(Q|R|A), ts[1]->child(R|A), ts[2]->child(Q|A), ts[3]->child(A)}}, v);
    }
    else if (std::all_of(ts.begin(), ts.end(),
        [](const XTree<3>* t){ return t->type == Interval::AMBIGUOUS &&
                                      !t->isBranch(); }))
    {
        // Sanity-checking that all cells have a Leaf struct allocated
        for (auto& t : ts)
        {
            assert(t->leaf != nullptr);
            (void)t;
        }
        /*  We need to check the values on the shared edge to see whether we need
         *  to add a face.  However, this is tricky when the edge spans multiple
         *  octree levels.
         *
         * In the following diagram, the target edge is marked with an o
         * (travelling out of the screen):
         *      _________________
         *      | 2 |           |
         *      ----o   1, 3    |  ^ R
         *      | 0 |           |  |
         *      ----------------|  --> Q
         *
         *  If we were to look at corners of c or d, we wouldn't be looking at the
         *  correct edge.  Instead, we need to look at corners for the smallest cell
         *  among the function arguments.
         */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const XTree<3>* a, const XTree<3>* b)
                { return a->leaf->level < b->leaf->level; }) - ts.begin();

        constexpr std::array<uint8_t, 4> corners = {{Q|R, R, Q, 0}};

        // If there is a sign change across the relevant edge, then call the
        // watcher with the segment corners (with proper winding order)
        auto a = ts[index]->cornerState(corners[index]);
        auto b = ts[index]->cornerState(corners[index] | A);
        if (a != b)
        {
            if (a != Interval::FILLED)
            {
                v.template load<A, 0>(ts, index);
            }
            else
            {
                v.template load<A, 1>(ts, index);
            }
        }
    }
}

template <typename V, Axis::Axis A>
void face3(const std::array<const XTree<3>*, 2> ts, V& v)
{
    if (std::any_of(ts.begin(), ts.end(),
        [](const XTree<3>* t){ return t->isBranch(); }))
    {
        constexpr auto Q = Axis::Q(A);
        constexpr auto R = Axis::R(A);

        for (unsigned k : {0, (int)Q, (int)R, Q|R})
        {
            face3<V, A>({{ts[0]->child(k|A), ts[1]->child(k)}}, v);
        }

        edge3<V, Q>({{ts[0]->child(A), ts[0]->child(R|A), ts[1]->child(0), ts[1]->child(R)}}, v);
        edge3<V, Q>({{ts[0]->child(Q|A), ts[0]->child(Q|R|A), ts[1]->child(Q), ts[1]->child(Q|R)}}, v);

        edge3<V, R>({{ts[0]->child(A), ts[1]->child(0), ts[0]->child(A|Q), ts[1]->child(Q)}}, v);
        edge3<V, R>({{ts[0]->child(R|A), ts[1]->child(R), ts[0]->child(R|A|Q), ts[1]->child(R|Q)}}, v);
    }
}

template <typename V, Axis::Axis A>
void call_edge3(const XTree<3>* t, V& v)
{
    for (auto a : {Axis::Axis(0), A})
    {
        edge3<V, A>({{t->child(a),
             t->child(Axis::Q(A) | a),
             t->child(Axis::R(A) | a),
             t->child(Axis::Q(A) | Axis::R(A) | a)}}, v);
    }
}

template <typename V, Axis::Axis A>
void call_face3(const XTree<3>* t, V& v)
{
    constexpr auto q = Axis::Q(A);
    constexpr auto r = Axis::R(A);

    face3<V, A>({{t->child(0), t->child(A)}}, v);
    face3<V, A>({{t->child(q), t->child(q|A)}}, v);
    face3<V, A>({{t->child(r), t->child(r|A)}}, v);
    face3<V, A>({{t->child(q|r), t->child(q|r|A)}}, v);
}

template <>
template <typename V>
void Dual<3>::walk(const XTree<3>* t, V& v, ProgressWatcher* progress)
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
        call_face3<V, Axis::X>(t, v);
        call_face3<V, Axis::Y>(t, v);
        call_face3<V, Axis::Z>(t, v);

        // Call the edge function 6 times (2x per axis)
        call_edge3<V, Axis::X>(t, v);
        call_edge3<V, Axis::Y>(t, v);
        call_edge3<V, Axis::Z>(t, v);
    }
    if (progress != nullptr)
    {
        progress->tick();
    }
}

}   // namespace Kernel
