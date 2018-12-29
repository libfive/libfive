/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>

#include "libfive/render/brep/dc/dc_mesher.hpp"
#include "libfive/render/brep/per_thread_brep.hpp"
#include "libfive/render/brep/dc/dc_tree.hpp"

namespace Kernel {

template <Axis::Axis A>
std::pair<int, bool> DCMesher::getIndexAndSign(
            const std::array<const DCTree<3>*, 4>& ts)
{
    // Exit immediately if we can prove that there will be no
    // face produced by this edge.
    if (std::any_of(ts.begin(), ts.end(),
        [](const DCTree<3>* t){ return t->type != Interval::AMBIGUOUS; }))
    {
        return {-1, false};
    }

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
            [](const DCTree<3>* a, const DCTree<3>* b)
            { return a->leaf->level < b->leaf->level; }) - ts.begin();

    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    constexpr std::array<uint8_t, 4> corners = {{Q|R, R, Q, 0}};

    // If there is a sign change across the relevant edge, then call the
    // watcher with the segment corners (with proper winding order)
    auto a = ts[index]->cornerState(corners[index]);
    auto b = ts[index]->cornerState(corners[index] | A);
    if (a != b)
    {
        if (a != Interval::FILLED) {
            return {index, false};
        } else {
            return {index, true};
        }
    }
    return {-1, false};
}


template <Axis::Axis A>
void DCMesher::load(const std::array<const DCTree<3>*, 4>& ts)
{
    auto index_sign = getIndexAndSign<A>(ts);
    if (index_sign.first == -1) {
        return;
    } else {
        assert(index_sign.first >= 0);
    }

    if (index_sign.second) {
        load<A, 1>(ts, index_sign.first);
    } else {
        load<A, 0>(ts, index_sign.first);
    }
}

template <Axis::Axis A, bool D>
void DCMesher::load(const std::array<const DCTree<3>*, 4>& ts, unsigned index)
{
    int es[4];
    {   // Unpack edge vertex pairs into edge indices
        auto q = Axis::Q(A);
        auto r = Axis::R(A);
        std::vector<std::pair<unsigned, unsigned>> ev = {
            {q | r, q | r | A},
            {r, r | A},
            {q, q | A},
            {0, A} };
        for (unsigned i=0; i < 4; ++i)
        {
            es[i] = MarchingTable<3>::mt.e[D ? ev[i].first  : ev[i].second]
                                   [D ? ev[i].second : ev[i].first];
            assert(es[i] != -1);
        }
    }

    uint32_t vs[4];
    Eigen::Matrix<double, 3, 4> vert_positions;
    for (unsigned i=0; i < ts.size(); ++i)
    {
        assert(ts[i]->leaf != nullptr);

        // Load either a patch-specific vertex (if this is a lowest-level,
        // potentially non-manifold cell) or the default vertex
        auto vi = ts[i]->leaf->level > 0
            ? 0
            : MarchingTable<3>::mt.p[ts[i]->leaf->corner_mask][es[i]];
        assert(vi != -1);

        // Sanity-checking manifoldness of collapsed cells
        assert(ts[i]->leaf->level == 0 || ts[i]->leaf->vertex_count == 1);

        if (ts[i]->leaf->index[vi] == 0)
        {
            ts[i]->leaf->index[vi] = m.pushVertex(ts[i]->vert(vi));
        }

        // Save the vertex position for normal calculations
        vert_positions.col(i) = ts[i]->vert(vi);

        // Store the vertex index for pushing triangles
        vs[i] = ts[i]->leaf->index[vi];
    }

#if LIBFIVE_TRIANGLE_FAN_MESHING
    // Get the intersection vertex.  Intersections in the vector are paired
    // as inside/outside, so we'll take both and average them.
    assert(ts[index]->intersection(es[index]).get() != nullptr);
    auto& intersectVec = *ts[index]->intersection(es[index]);
    assert(intersectVec.size() >= 2);
    if (intersectVec[0].index == 0)
    {
        auto intersectPos1 = intersectVec[0].pos;
        auto intersectPos2 = intersectVec[1].pos;
        for (unsigned i=2; i < intersectVec.size(); ++i)
        {
            assert(intersectVec[i].pos == intersectVec[i % 2].pos);
        }
        auto intersectPos = (intersectPos1 + intersectPos2) / 2.;
        intersectVec[0].index = m.pushVertex(intersectPos);
    }
    auto vCenter = intersectVec[0].index;
    assert(vCenter != 0);

    // Rather than triangulating our quad into two triangles, we triangulate
    // into four triangles, sharing a vertex at our computed intersection.
    // This gives us greater accuracy at no extra computation cost, and (more
    // importantly) will allow us to ensure that every triangle is
    // contained in the cells that generated it (provided that its endpoints
    // are in their proper cells), greatly reducing the
    // opportunities for self-intersection.

    // Handle polarity-based windings
    constexpr auto idx1 = D ? 1 : 2;
    constexpr auto idx2 = D ? 2 : 1;
    constexpr auto A01 = D ? Q(A) : R(A);
    constexpr auto A02 = D ? R(A) : Q(A);

    checkAndAddTriangle<A01, true>(ts[0], ts[idx1], vs[0], vs[idx1], vCenter);
    checkAndAddTriangle<A02, true>(ts[idx1], ts[3], vs[idx1], vs[3], vCenter);
    checkAndAddTriangle<A01, false>(ts[3], ts[idx2], vs[3], vs[idx2], vCenter);
    checkAndAddTriangle<A02, false>(ts[idx2], ts[0], vs[idx2], vs[0], vCenter);

#else
    (void)index; // This variable is unused in non-fan meshing

    // Handle polarity-based windings
    if (!D)
    {
        std::swap(vs[1], vs[2]);

        const Eigen::Vector3d r = vert_positions.col(1);
        vert_positions.col(1) = vert_positions.col(2);
        vert_positions.col(2) = r;
    }
    // Pick a triangulation that prevents triangles from folding back
    // on each other by checking normals.
    std::array<Eigen::Vector3d, 4> norms;

    // Computes and saves a corner normal.  a,b,c must be right-handed
    // according to the quad winding, which looks like
    //     2---------3
    //     |         |
    //     |         |
    //     0---------1
    auto saveNorm = [&](int a, int b, int c){
        norms[a] = (vert_positions.col(b) - vert_positions.col(a)).cross
                   (vert_positions.col(c) - vert_positions.col(a)).normalized();
    };
    saveNorm(0, 1, 2);
    saveNorm(1, 3, 0);
    saveNorm(2, 0, 3);
    saveNorm(3, 2, 1);

    // Helper function to push triangles that aren't simply lines
    auto push_triangle = [&](uint32_t a, uint32_t b, uint32_t c) {
        if (a != b && b != c && a != c)
        {
            m.branes.push_back({a, b, c});
        }
    };

    if (norms[0].dot(norms[3]) > norms[1].dot(norms[2]))
    {
        push_triangle(vs[0], vs[1], vs[2]);
        push_triangle(vs[2], vs[1], vs[3]);
    }
    else
    {
        push_triangle(vs[0], vs[1], vs[3]);
        push_triangle(vs[0], vs[3], vs[2]);
    }
#endif
}

#if LIBFIVE_TRIANGLE_FAN_MESHING
template <Axis::Axis A, bool D>
void DCMesher::checkAndAddTriangle(const DCTree<3>* a, const DCTree<3>* b,
                                   uint32_t aIndex, uint32_t bIndex,
                                   uint32_t intersectionIndex)
{
    if (a == b)
    {
        // No triangle belongs here.
        return;
    }
    // If either cell does not actually contain its vertex, there is no way
    // to make the resulting triangles be in only the generating cells;
    // however, we can at least keep it from crossing more axes than the
    // vertices of a and b force it to.  (If they force it to cross the
    // plane between a and b in the wrong direction or not at all, though,
    // things are likely hopeless and so we just add (a,b,intersection).

    // If the cells are the same size, adding (a,b,intersection) will always
    // be sufficient; otherwise, more complicated steps are needed to make
    // that determination.

    if (a->level() != b->level())
    {
        const auto& aPos = m.verts[aIndex];
        const auto& bPos = m.verts[bIndex];
        const auto& intersectionPos = m.verts[intersectionIndex];
        const auto& boundaryValue = intersectionPos[Axis::toIndex(A)];

        const auto aIsSmaller = a->level() < b->level();
        const auto smallerCell = aIsSmaller ? a : b;
        const auto& smallerRegion = (smallerCell)->region;
        const auto& smallerVertex = aIsSmaller ? aPos : bPos;
        const auto& largerVertex = aIsSmaller ? bPos : aPos;
        const auto largerDirection = (aIsSmaller == D) ? 1 : -1;
        auto compareToBounds = [&smallerRegion]
        (const Eigen::Vector3d& pt, Axis::Axis compAxis)
        {
            auto idx = Axis::toIndex(compAxis);
            if (pt[idx] < smallerRegion.lower[idx])
            {
                return -1;
            }
            else if (pt[idx] > smallerRegion.upper[idx])
            {
                return 1;
            }
            else
            {
                return 0;
            }
        };


        if (compareToBounds(smallerVertex, A) != largerDirection &&
            compareToBounds(largerVertex, A) == largerDirection)
        {

            // Find where the line between the vertices of a and b
            // intersects the plane of the boundary between a and b.
            if (D)
            {
                assert(aPos[Axis::toIndex(A)] < boundaryValue);
                assert(bPos[Axis::toIndex(A)] > boundaryValue);
            }
            else
            {
                assert(aPos[Axis::toIndex(A)] > boundaryValue);
                assert(bPos[Axis::toIndex(A)] < boundaryValue);
            }
            auto aWeight = std::abs(bPos[Axis::toIndex(A)] - boundaryValue);
            auto bWeight = std::abs(aPos[Axis::toIndex(A)] - boundaryValue);
            auto weightSum = aWeight + bWeight;
            auto aFactor = aWeight / weightSum;
            auto bFactor = bWeight / weightSum;
            auto crossing = (aFactor * aPos + bFactor * bPos).eval();
            assert(std::abs(crossing[Axis::toIndex(A)] -
                            intersectionPos[Axis::toIndex(A)]) < 1e-6f);
            crossing[Axis::toIndex(A)] = intersectionPos[Axis::toIndex(A)];

            // Determine on which side(s) of the actual boundary the crossing is.

            auto Q = Axis::Q(A);
            auto R = Axis::R(A);
            auto compareCrossing =
                [&, compareToBounds](Axis::Axis compAxis)
            {
                auto out = compareToBounds(crossing, compAxis);
                auto getBound = [&out, &compAxis](const DCTree<3>* cell)
                {
                    return out == 1
                        ? cell->region.upper[Axis::toIndex(compAxis)]
                        : cell->region.lower[Axis::toIndex(compAxis)];
                };
                if (out != 0 &&
                        (out == compareToBounds(smallerVertex, compAxis) ||
                            (getBound(a) == getBound(b) &&
                             out == compareToBounds(largerVertex, compAxis))))
                {
                    return 0;
                }
                else
                {
                    return out;
                }
            };

            auto QCrossingSide = compareCrossing(Q);
            auto RCrossingSide = compareCrossing(R);

            // If both are inside the region, the triangle (a,b,intersection)
            // will be inside the appropriate cells (or at least not cross
            // more boundaries than necessary), so we can skip to adding
            // that.  Otherwise, more steps need to be taken.

            if (QCrossingSide != 0 || RCrossingSide != 0)
            {
                // If we've already found the appropriate forced vertex for aIndex
                // and bIndex, use it; otherwise, find it and add it.

                const std::pair<uint32_t, uint32_t>
                    pair{ std::min(aIndex, bIndex), std::max(aIndex, bIndex) };
                auto& forcedIndex = forcedVerts[pair];
                if (forcedIndex == 0)
                {
                    // Each comparison shows the crossing to be beyond at most one
                    // edge of the smaller cell that adjoins the larger cell.  We
                    // can find the intersection on that edge, if there is one.
                    auto getIntersection =
                        [&aIsSmaller, &smallerCell, &aIndex, &bIndex]
                    (Axis::Axis compAxis, int compared)->IntersectionVec<3>*
                    {
                        assert(compAxis != A);
                        if (compared == 0)
                        {
                            return nullptr;
                        }
                        else
                        {
                            // Find the edge.
                            auto boundaryTerm = (D == aIsSmaller) ? A : 0;
                            auto compTerm = compared > 0 ? compAxis : 0;
                            auto thirdTerm = 7 - A - compAxis;
                            auto vert1 = boundaryTerm | compTerm;
                            auto vert2 = boundaryTerm | compTerm | thirdTerm;
                            auto edge = MarchingTable<3>::mt.e[vert1][vert2];
                            assert(edge != -1);
                            auto patch = MarchingTable<3>::mt.
                                p[smallerCell->leaf->corner_mask][edge];
                            if (patch == -1)
                            {
                                edge = MarchingTable<3>::mt.e[vert2][vert1];
                                assert(edge != -1);
                                patch = MarchingTable<3>::mt.
                                    p[smallerCell->leaf->corner_mask][edge];
                            }
                            // We have a patch value (possibly -1), but we only
                            // want to use this edge if it's the patch of
                            // smallerCell that we're actually using.  (The
                            // bigger cell can only have one patch, since it
                            // has positive level.)
                            if (smallerCell->leaf->index[patch] ==
                                (aIsSmaller ? aIndex : bIndex))
                            {
                                assert(smallerCell->intersection(edge));
                                return smallerCell->intersection(edge)
                                    .get();
                            }
                            else
                            {
                                return nullptr;
                            }
                        }
                    };
                    auto QIntersection = getIntersection(Q, QCrossingSide);
                    auto RIntersection = getIntersection(R, RCrossingSide);
                    if ((QIntersection == nullptr) ==
                        (RIntersection == nullptr))
                    {
                        // We have either no usable intersection or too many,
                        // so just use the point in our boundary closest
                        // to the intersection.
                        if (QCrossingSide < 0)
                        {
                            crossing[Q] = smallerRegion.lower[Q];
                        }
                        else if (QCrossingSide > 0)
                        {
                            crossing[Q] = smallerRegion.upper[Q];
                        }
                        if (RCrossingSide < 0)
                        {
                            crossing[R] = smallerRegion.lower[R];
                        }
                        else if (RCrossingSide > 0)
                        {
                            crossing[R] = smallerRegion.upper[R];
                        }
                        forcedIndex = m.pushVertex(crossing);
                    }
                    else
                    {
                        auto intersectionVec =
                            QIntersection ? QIntersection : RIntersection;
                        assert(intersectionVec != nullptr);
                        assert(intersectionVec->size() >= 2);
                        if ((*intersectionVec)[0].index == 0)
                        {
                            (*intersectionVec)[0].index = m.verts.size();
                            auto intersectPos1 = (*intersectionVec)[0].pos;
                            auto intersectPos2 = (*intersectionVec)[1].pos;
                            for (unsigned i = 2;
                                 i < intersectionVec->size();
                                 ++i)
                            {
                                assert((*intersectionVec)[i].pos ==
                                    (*intersectionVec)[i % 2].pos);
                            }
                            auto intersectPos =
                                (intersectPos1 + intersectPos2) / 2.;
                            m.verts.push_back(intersectPos);
                        }
                        forcedIndex = (*intersectionVec)[0].index;
                        if ((*intersectionVec)[0].index == intersectionIndex)
                        {
                            // The resulting triangles will be degenerate,
                            // so we return without adding them.
                            return;
                        }
                    }
                }
                assert(forcedIndex != 0);
                m.branes.push_back({ intersectionIndex, aIndex, forcedIndex });
                m.branes.push_back({ bIndex, intersectionIndex, forcedIndex });
                return;
            }
        }
    }
    // Just add the triangle (a,b,intersection).
    m.branes.push_back({ aIndex, bIndex, intersectionIndex });
}
#endif

////////////////////////////////////////////////////////////////////////////////

// Explicit template instantiation
template void DCMesher::load<Axis::X>(const std::array<const DCTree<3>*, 4>&);
template void DCMesher::load<Axis::Y>(const std::array<const DCTree<3>*, 4>&);
template void DCMesher::load<Axis::Z>(const std::array<const DCTree<3>*, 4>&);

}   // namespace Kernel
