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
#include "libfive/render/brep/eval_xtree.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

struct Corner
{
    unsigned index;
    Simplex<2> simplex;
};

struct RunData
{
    XTreeEvaluator* eval;
    unsigned min_depth;
    unsigned max_depth;
    double max_err;
    BRep<2> out;
};

////////////////////////////////////////////////////////////////////////////////
// Forward declarations
void recurse(SimplexTree<2>* t, RunData& data);
void refine(SimplexTree<2>* t, RunData& data);
void cell(std::unique_ptr<SimplexTree<2>>& t, Region<2> region, unsigned depth,
          RunData& data);

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <Axis::Axis A>
void edge2(const std::array<SimplexTree<2>*, 2>& ts, RunData& data)
{
    if (A == Axis::X)
    {
        assert(ts[0]->region.upper.y() == ts[1]->region.lower.y());
    }
    else if (A == Axis::Y)
    {
        assert(ts[0]->region.upper.x() == ts[1]->region.lower.x());
    }
    assert(ts[0]->type != Interval::UNKNOWN);
    assert(ts[1]->type != Interval::UNKNOWN);

    constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;

    // Stop recursing if every tree is filled or empty
    if (std::all_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->type == Interval::FILLED; }) ||
        std::all_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->type == Interval::EMPTY; }))
    {
        return;
    }

    // Refine trees and recall edge2 if either tree is incomplete
    if (std::any_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return !t->complete; }))
    {
        for (auto& t : ts)
        {
            if (!t->complete)
            {
                refine(t, data);
            }
        }
        edge2<A>(ts, data);
        return;
    }

    // Continue recursing if either tree is a branch node.
    if (std::any_of(ts.begin(), ts.end(),
        [](const SimplexTree<2>* t){ return t->isBranch(); }))
    {
        edge2<A>({{ts[0]->child(perp), ts[1]->child(0)}}, data);
        edge2<A>({{ts[0]->child(A|perp), ts[1]->child(A)}}, data);
        return;
    }

    // Pick the index of the minimum edge, which is the one that we'll
    // use to build the four triangles to run marching triangles over.
    const unsigned index = std::max_element(ts.begin(), ts.end(),
            [](const SimplexTree<2>* a, const SimplexTree<2>* b)
            { return a->depth < b->depth; }) - ts.begin();

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
     *      Or, along the other axis:
     *
     *      ---------
     *      |   4   |
     *      | /2|1\ |   ^
     *      0---2---1   | perp
     *      | \3|0/ |   |
     *      |   3   |   ----> A
     *      ---------
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

    // This hardcodes the four triangles to run MT over
    const std::array<std::array<unsigned, 3>, 4> tris = {{
        {{2,1,3}},
        {{2,4,1}},
        {{2,0,4}},
        {{2,3,0}},
    }};

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
        {{0, 0, 0, 0}}  // All filled (invalid)
    }};

    for (const auto& t : tris)
    {
        unsigned mask = 0;
        for (unsigned i=0; i < 3; ++i)
        {
            auto c = corners[t[i]];
            mask |= ts[c.index]->inside[c.simplex.toIndex()] ? (1 << i) : 0;
        }

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
            es.col(i) = ts[c.index]->vertices.col(c.simplex.toIndex());
        }

        // Confirm the inside / outside state of the vertices
        assert(es(2, 0) <= 0);
        assert(es(2, 1) >= 0);
        assert(es(2, 2) <= 0);
        assert(es(2, 3) >= 0);

        // Do a linear search along both edges to find the intersection
        constexpr unsigned count=32;
        static_assert(2 * count < LIBFIVE_EVAL_ARRAY_SIZE, "Bad search size");
        for (unsigned i=0; i < count; ++i)
        {
            Eigen::Vector3f tmp;
            tmp << ((es.col(0) * (1 - i / double(count - 1))) +
                    (es.col(1) * i / double(count - 1))).head<2>().cast<float>(),
                ts[0]->region.perp.cast<float>();
            data.eval->array.set(tmp, i);
            tmp << ((es.col(2) * (1 - i / double(count - 1))) +
                    (es.col(3) * i / double(count - 1))).head<2>().cast<float>(),
                ts[1]->region.perp.cast<float>();
            data.eval->array.set(tmp, i + count);
        }
        auto r = data.eval->array.values(2 * count);

        Eigen::Vector2d a, b;
        for (unsigned i=0; i < count - 1; ++i)
        {
            if (r[i] <= 0 && r[i + 1] >= 0 &&
               (r[i] < 0 || r[i + 1] > 0))
            {
                a = ((es.col(0) * (1 - i / double(count - 1))) +
                    (es.col(1) * i / double(count - 1))).head<2>();
                break;
            }
        }
        for (unsigned i=0; i < count - 1; ++i)
        {
            if (r[i + count] <= 0 && r[i + count + 1] >= 0 &&
               (r[i + count] < 0 || r[i + count + 1] > 0))
            {
                b = ((es.col(2) * (1 - i / double(count - 1))) +
                     (es.col(3) * i / double(count - 1))).head<2>();
                break;
            }
        }

        auto ai = data.out.verts.size();
        data.out.verts.push_back(a.cast<float>());
        auto bi = data.out.verts.size();
        data.out.verts.push_back(b.cast<float>());
        data.out.branes.push_back({ai, bi});
    }
}

void recurse(SimplexTree<2>* t, RunData& data)
{
    auto subregions = t->region.subdivide();
    for (unsigned i=0; i < t->children.size(); ++i)
    {
        cell(t->children[i], subregions[i], t->depth + 1, data);
    }

    // Attempt to refine the interval type of the parent tree,
    // because interval arithemtic gets more precise on smaller
    // regions and we may be able to prove that it is empty / filled.
    if (t->type == Interval::AMBIGUOUS)
    {
        bool all_filled = true;
        bool all_empty = true;
        for (const auto& c : t->children)
        {
            all_filled &= (c->type == Interval::FILLED);
            all_empty &=  (c->type == Interval::EMPTY);
        }
        t->type = all_filled ? Interval::FILLED :
                  all_empty  ? Interval::EMPTY  : t->type;
    }

    //  Then, call edge on every pair of cells
    edge2<Axis::Y>({{t->child(0), t->child(Axis::X)}}, data);
    edge2<Axis::Y>({{t->child(Axis::Y), t->child(Axis::Y | Axis::X)}}, data);
    edge2<Axis::X>({{t->child(0), t->child(Axis::Y)}}, data);
    edge2<Axis::X>({{t->child(Axis::X), t->child(Axis::X | Axis::Y)}}, data);
}

void refine(SimplexTree<2>* t, RunData& data)
{
    const auto err = t->findVertices(data.eval);
    std::cout << "GOt error " << err << std::endl;
    if (err > data.max_err && t->depth < data.max_depth && !t->isBranch())
    {
        recurse(t, data);
    }
    else
    {
        t->checkVertices(data.eval);
    }
    t->complete = true;
}

void cell(std::unique_ptr<SimplexTree<2>>& t, Region<2> region, unsigned depth,
          RunData& data)
{
    assert(t.get() == nullptr);
    t.reset(new SimplexTree<2>(data.eval, region, depth));
    assert(t->type != Interval::UNKNOWN);

    //  Return early if this region is entirely empty or filled
    //  Otherwise, force recursion down to a minimum depth
    if (t->type == Interval::AMBIGUOUS && depth < data.min_depth)
    {
        recurse(t.get(), data);
    }

    data.eval->interval.pop();
}

std::pair<BRep<2>, std::unique_ptr<SimplexTree<2>>> walk2d(
        XTreeEvaluator* eval, Region<2> region,
        unsigned min_depth, unsigned max_depth, double max_err)
{
    RunData data;
    data.eval = eval;
    data.min_depth = min_depth;
    data.max_depth = max_depth;
    data.max_err = max_err;

    std::unique_ptr<SimplexTree<2>> t;
    cell(t, region, 0, data);
    return std::make_pair(data.out, std::move(t));
}

}   // namespace Kernel
