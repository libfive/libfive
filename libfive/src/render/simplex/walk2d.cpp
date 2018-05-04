#include <iostream>
/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018 Matt Keeter

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
#include "libfive/render/simplex/simplextree.hpp"
#include "libfive/render/simplex/simplex.hpp"
#include "libfive/render/simplex/walk2d.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

struct Corner
{
    unsigned index;
    Simplex<2> simplex;
};

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <Axis::Axis A>
void edge2(const std::array<const SimplexTree<2>*, 2>& ts, BRep<2>& out)
{
    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;

    if (std::any_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->isBranch(); }))
    {
        edge2<A>({{ts[0]->child(perp), ts[1]->child(0)}}, out);
        edge2<A>({{ts[0]->child(A|perp), ts[1]->child(A)}}, out);
    }
    else if (std::any_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->type == Interval::AMBIGUOUS; }))
    {
        // Pick the index of the minimum edge, which is the one that we'll
        // use to build the four triangles to run marching triangles over.
        const unsigned index = std::max_element(ts.begin(), ts.end(),
                [](const SimplexTree<2>* a, const SimplexTree<2>* b)
                { return a->depth < b->depth ||
                         (b->type == Interval::AMBIGUOUS &&
                          a->type != Interval::AMBIGUOUS); }) - ts.begin();
        std::cout << "index: " << index << " axis: " << A << "\n";
        std::cout << ts[0]->depth << " " << ts[1]->depth << "\n";

        /*
         *      Given two adjacent squares, here's how we order points
         *      (on simplex vertices) and triangles (between points):
         *
         *      -------1-------
         *      |    /0|1\    |   ^
         *      |   3--2--4   |   | A
         *      |    \3|2/    |   |
         *      -------0-------   ----> perp
         *
         */
        std::array<Corner, 5> corners = {{
            {index, Simplex<2>::fromCorner(index ? 0 : perp)},
            {index, Simplex<2>::fromCorner((index ? 0 : perp) | A)},
            {index, Simplex<2>::merge(
                    Simplex<2>::fromCorner(index ? 0 : perp),
                    Simplex<2>::fromCorner((index ? 0 : perp) | A))},
            {0, Simplex<2>::fromIndex(8)},
            {1, Simplex<2>::fromIndex(8)}
        }};

        std::cout << "corners:\n";
        for (auto& c : corners)
        {
            std::cout << ts[c.index]->vertices.col(c.simplex.toIndex()).transpose() << "    " << ts[c.index]->inside[c.simplex.toIndex()] << "\n";
        }
        std::cout << "\n";

        // This hardcodes the four triangles to run MT over
        const std::array<std::array<unsigned, 3>, 4> tris =
            {{{{2,1,3}}, {{2,4,1}}, {{2,0,4}}, {{2,3,0}}}};

        /*
         *  Given a triangle with corners
         *      2
         *     /|
         *    / |
         *   0--1
         *
         *  We encode the 8 possible bitfield patterns and
         *  a list of pairs of edges between which we draw lines,
         *  with {-1,-1} as the null edge
         */
        const std::array<std::array<unsigned, 4>, 8> table = {{
            {{0, 0, 0, 0}}, // All empty (invalid)
            {{0, 1, 0, 2}}, // 0
            {{1, 2, 1, 0}}, // 1
            {{1, 2, 0, 2}}, // 1 + 0
            {{2, 0, 2, 1}}, // 2
            {{0, 1, 2, 1}}, // 2 + 0
            {{2, 0, 1, 0}}, // 2 + 1
            {{0, 0, 0, 0}} // All filled (invalid)
        }};

        for (const auto& t : tris)
        {
            unsigned mask = 0;
            for (unsigned i=0; i < 3; ++i)
            {
                auto c = corners[t[i]];
                mask |= ts[c.index]->inside[c.simplex.toIndex()] ? (1 << i) : 0;
            }

            std::cout << "mask: " << (unsigned)mask << "  tri: " << (&t - tris.data()) << "\n";
            // Skip the empty or filled triangles
            if (mask == 0 || mask == 7)
            {
                continue;
            }

            // Pick out the two edges to search, storing their start + end
            // vertices in an matrix here.
            assert(mask <= table.size());
            const auto entry = table[mask];
            Eigen::Matrix<double, 3, 4> es;
            for (unsigned i=0; i < 4; ++i)
            {
                assert(entry[i] < t.size());
                assert(t[entry[i]] < corners.size());

                auto c = corners[t[entry[i]]];
                std::cout << entry[i] << " " << t[entry[i]] << " " << c.index << " " << c.simplex.toIndex() << "\n";
                es.col(i) = ts[c.index]->vertices.col(c.simplex.toIndex());
            }

            std::cout << "ts[0].vertices:\n" << ts[0]->vertices << "(" << ts[0]->type << ")\n";
            std::cout << "ts[1].vertices:\n" << ts[1]->vertices << "(" << ts[1]->type << ")\n";
            // Confirm the inside / outside state of the vertices
            std::cout << "es:\n" << es << "\n";
            assert(es(2, 0) >= 0);
            assert(es(2, 1) <= 0);
            assert(es(2, 2) >= 0);
            assert(es(2, 3) <= 0);

            // Do a linear search along both edges to find the intersection
            auto t0 = es(2, 1) / (es(2, 1) - es(2, 0));
            assert(t0 >= 0);
            assert(t0 <= 1);
            Eigen::Vector2d a = (t0 * es.col(0) + (1 - t0) * es.col(1))
                .head<2>();

            double t1 = es(2, 3) / (es(2, 3) - es(2, 2));
            assert(t1 >= 0.0);
            assert(t1 <= 1.0);
            Eigen::Vector2d b = (t1 * es.col(2) + (1 - t1) * es.col(3))
                .head<2>();

            auto ai = out.verts.size();
            out.verts.push_back(a.cast<float>());
            auto bi = out.verts.size();
            out.verts.push_back(b.cast<float>());
            out.branes.push_back({ai, bi});
            std::cout << "\n";
        }

        std::cout << "----------------------------------------------------------------------\n";
    }
}

void vert2d(const SimplexTree<2>* t, BRep<2>& out)
{
    if (t->isBranch())
    {
        // Recurse down every subface in the quadtree
        for (unsigned i=0; i < t->children.size(); ++i)
        {
            auto c = t->child(i);
            if (c != t)
            {
                vert2d(c, out);
            }
        }

        //  Then, call edge on every pair of cells
        edge2<Axis::Y>({{t->child(0), t->child(Axis::X)}}, out);
        edge2<Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, out);
        edge2<Axis::X>({{t->child(0), t->child(Axis::Y)}}, out);
        edge2<Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, out);
    }
}

BRep<2> walk2d(const SimplexTree<2>* t)
{
    BRep<2> out;
    vert2d(t, out);
    return out;
}

}   // namespace Kernel
