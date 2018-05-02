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
#pragma once

#include "libfive/render/simplex/simplextree.hpp"
#include "libfive/render/simplex/simplex.hpp"
#include "libfive/render/simplex/dual.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

typedef SimplexDual<2>::Corner  Corner;
typedef SimplexDual<2>::Prim    Prim;

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <typename V, Axis::Axis A>
void edge2(const std::array<const SimplexTree<2>*, 2>& ts, V& v)
{
    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;

    if (std::any_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->isBranch(); }))
    {
        edge2<V, A>({{ts[0]->child(perp), ts[1]->child(0)}}, v);
        edge2<V, A>({{ts[0]->child(A|perp), ts[1]->child(A)}}, v);
    }
    else
    {
        // Pick the index of the minimum edge, which is the one that we'll
        // use to build the four triangles to run marching triangles over.
        const auto index = std::max_element(ts.begin(), ts.end(),
                [](const SimplexTree<2>* a, const SimplexTree<2>* b)
                { return a->depth < b->depth; }) - ts.begin();

        /*
         *      Given two adjacent squares, here's how we order points:
         *
         *      -------b-------
         *      |    / | \    |
         *      |  c0--e--c1  |
         *      |    \ | /    |
         *      -------a-------
         *
         *      or, packed into an array,
         *
         *      -------1-------
         *      |    / | \    |
         *      |   3--2--4   |
         *      |    \ | /    |
         *      -------0-------
         */
        Corner a = {index, Simplex<2>::fromCorner(index ? 0 : perp)};
        Corner b = {index, Simplex<2>::fromCorner((index ? 0 : perp) | A)};
        Corner e = {index, Simplex<2>::merge(a.simplex, b.simplex)};
        Corner c0 = {0, Simplex<2>::fromIndex(7)};
        Corner c1 = {0, Simplex<2>::fromIndex(7)};

        // Pack these corners into an array for ease of indexing
        std::array<Corner, 5> corners = {{a, b, e, c0, c1}};

        // This hardcodes the four triangles to run MT over
        const std::array<std::array<unsigned, 3>, 4> tris =
            {{{{2,1,3}}, {{2,4,1}}, {{2,0,4}}, {{2,3,0}}}};

        for (const auto& t : tris)
        {
            unsigned mask = 0;
            for (unsigned i=0; i < 3; ++i)
            {
                auto c = corners[t[i]];
                mask |= ts[c.index]->inside[c.simplex.toIndex()] ? (1 << i) : 0;
            }
            // Then run marching triangles based on the mask.  Easy!
        }
    }
}

template <>
template <typename V>
void SimplexDual<2>::walk(const SimplexTree<2>* t, V& v)
{
    if (t->isBranch())
    {
        // Recurse down every subface in the quadtree
        for (unsigned i=0; i < t->children.size(); ++i)
        {
            auto c = t->child(i);
            if (c != t)
            {
                walk(c, v);
            }
        }

        //  Then, call edge on every pair of cells
        edge2<V, Axis::Y>({{t->child(0), t->child(Axis::X)}}, v);
        edge2<V, Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, v);
        edge2<V, Axis::X>({{t->child(0), t->child(Axis::Y)}}, v);
        edge2<V, Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, v);
    }
}

}   // namespace Kernel
