/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <numeric>
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "libfive/render/brep/dc/mesh.hpp"
#include "libfive/render/brep/dc/xtree_pool.hpp"
#include "libfive/render/brep/dc/dual.hpp"
#include "libfive/render/brep/dc/intersection_aligner.hpp"

#ifndef LIBFIVE_TRIANGLE_FAN_MESHING
#define LIBFIVE_TRIANGLE_FAN_MESHING 0
#endif

namespace Kernel {

const float Mesh::MAX_PROGRESS = 3.0f;

template <Axis::Axis A>
void Mesh::load(const std::array<const XTree<3>*, 4>& ts, unsigned index)
{
    constexpr auto Q = Axis::Q(A);
    constexpr auto R = Axis::R(A);

    constexpr std::array<uint8_t, 4> corners = {{Q|R, R, Q, 0}};

    // If there is a sign change across the relevant edge, then call the
    // watcher with the segment corners (with proper winding order)
    auto a = ts[index]->cornerState(corners[index]);
    auto b = ts[index]->cornerState(corners[index] | A);
    if (a != b)
    {
        if (a != Interval::FILLED)
        {
            load<A, 0>(ts);
        }
        else
        {
            load<A, 1>(ts);
        }
    }
}

template <Axis::Axis A, bool D>
void Mesh::load(const std::array<const XTree<3>*, 4>& ts)
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
            es[i] = XTree<3>::mt->e[D ? ev[i].first  : ev[i].second]
                                   [D ? ev[i].second : ev[i].first];
            assert(es[i] != -1);
        }
    }

    uint32_t vs[4];
    for (unsigned i=0; i < ts.size(); ++i)
    {
        assert(ts[i]->leaf != nullptr);

        // Load either a patch-specific vertex (if this is a lowest-level,
        // potentially non-manifold cell) or the default vertex
        auto vi = ts[i]->leaf->level > 0
            ? 0
            : XTree<3>::mt->p[ts[i]->leaf->corner_mask][es[i]];
        assert(vi != -1);

        // Sanity-checking manifoldness of collapsed cells
        assert(ts[i]->leaf->level == 0 || ts[i]->leaf->vertex_count == 1);

        if (ts[i]->leaf->index[vi] == 0)
        {
            ts[i]->leaf->index[vi] = verts.size();

            verts.push_back(ts[i]->vert(vi).template cast<float>());
        }
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
        intersectVec[0].index = verts.size();
        verts.push_back(intersectPos.template cast<float>());
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
    }
    // Pick a triangulation that prevents triangles from folding back
    // on each other by checking normals.
    std::array<Eigen::Vector3f, 4> norms;
    auto saveNorm = [&](int a, int b, int c){
        norms[a] = (verts[vs[b]] - verts[vs[a]]).cross
                   (verts[vs[c]] - verts[vs[a]]).normalized();
    };

    // Computes and saves a corner normal.  a,b,c must be right-handed
    // according to the quad winding, which looks like
    //     2---------3
    //     |         |
    //     |         |
    //     0---------1
    saveNorm(0, 1, 2);
    saveNorm(1, 3, 0);
    saveNorm(2, 0, 3);
    saveNorm(3, 2, 1);

    // Helper function to push triangles that aren't simply lines
    auto push_triangle = [&](uint32_t a, uint32_t b, uint32_t c) {
        if (a != b && b != c && a != c)
        {
            branes.push_back({a, b, c});
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

template <Axis::Axis A, bool D>
void Mesh::checkAndAddTriangle(const XTree<3>* a, const XTree<3>* b,
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
        const auto& aPos = verts[aIndex];
        const auto& bPos = verts[bIndex];
        const auto& intersectionPos = verts[intersectionIndex];
        const auto& boundaryValue = intersectionPos[Axis::toIndex(A)];

        const auto aIsSmaller = a->level() < b->level();
        const auto smallerCell = aIsSmaller ? a : b;
        const auto& smallerRegion = (smallerCell)->region;
        const auto& smallerVertex = aIsSmaller ? aPos : bPos;
        const auto& largerVertex = aIsSmaller ? bPos : aPos;
        const auto largerDirection = (aIsSmaller == D) ? 1 : -1;
        auto compareToBounds = [&smallerRegion]
        (const Eigen::Vector3f& pt, Axis::Axis compAxis)
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
                auto getBound = [&out, &compAxis](const XTree<3>* cell)
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
                            auto edge = XTree<3>::mt->e[vert1][vert2];
                            assert(edge != -1);
                            auto patch = XTree<3>::mt->
                                p[smallerCell->leaf->corner_mask][edge];
                            if (patch == -1)
                            {
                                edge = XTree<3>::mt->e[vert2][vert1];
                                assert(edge != -1);
                                patch = XTree<3>::mt->
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
                        forcedIndex = verts.size();
                        verts.push_back(crossing);
                    }
                    else
                    {
                        auto intersectionVec =
                            QIntersection ? QIntersection : RIntersection;
                        assert(intersectionVec != nullptr);
                        assert(intersectionVec->size() >= 2);
                        if ((*intersectionVec)[0].index == 0)
                        {
                            (*intersectionVec)[0].index = verts.size();
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
                            verts.push_back(intersectPos
                                            .template cast<float>());
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
                branes.push_back({ intersectionIndex, aIndex, forcedIndex });
                branes.push_back({ bIndex, intersectionIndex, forcedIndex });
                return;
            }
        }
    }
    // Just add the triangle (a,b,intersection).
    branes.push_back({ aIndex, bIndex, intersectionIndex });
}

////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Mesh> Mesh::render(const Tree t, const Region<3>& r,
                                   double min_feature, double max_err,
                                   bool multithread,
                                   ProgressCallback progress_callback)
{
    std::atomic_bool cancel(false);
    std::map<Tree::Id, float> vars;
    return render(t, vars, r, min_feature, max_err,
                  multithread ? 8 : 1, cancel, progress_callback);
}

std::unique_ptr<Mesh> Mesh::render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback)
{
    std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
    es.reserve(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        es.emplace_back(XTreeEvaluator(t, vars));
    }

    return render(es.data(), r, min_feature, max_err, workers, cancel,
                  progress_callback);
}

std::unique_ptr<Mesh> Mesh::render(
        XTreeEvaluator* es,
        const Region<3>& r, double min_feature, double max_err,
        int workers, std::atomic_bool& cancel,
        ProgressCallback progress_callback)
{
    auto t = XTreePool<3>::build(es, r, min_feature, max_err, workers, cancel,
                                 progress_callback);
    auto out = mesh(t, cancel, progress_callback);
    t.reset(progress_callback);

    return out;
}

std::unique_ptr<Mesh> Mesh::mesh(const Root<XTree<3>, XTree<3>::Leaf>& xtree,
                                 std::atomic_bool& cancel,
                                 ProgressCallback progress_callback)
{
    if (cancel.load() || xtree.get() == nullptr)
    {
        return nullptr;
    }

#if LIBFIVE_TRIANGLE_FAN_MESHING
    // Make sure each intersection has the same object in all cells.
    IntersectionAligner aligner;
    Dual<3>::walk(xtree.get(), aligner, nullptr);
#endif

    // Perform marching squares
    auto m = std::unique_ptr<Mesh>(new Mesh());

    if (cancel.load() || xtree.get() == nullptr)
    {
        return nullptr;
    }
    else
    {
        std::atomic_bool done(false);
        auto progress_watcher = ProgressWatcher::build(
                xtree.size(), 1.0f,
                progress_callback, done, cancel);

        Dual<3>::walk(xtree.get(), *m, progress_watcher);

        done.store(true);
        delete progress_watcher;

#if DEBUG_OCTREE_CELLS
        // Store octree cells as lines
        std::list<const XTree<3>*> todo = {xtree.get()};
        while (todo.size())
        {
            auto t = todo.front();
            todo.pop_front();
            if (t->isBranch())
                for (auto& c : t->children)
                    todo.push_back(c.get());

            static const std::vector<std::pair<uint8_t, uint8_t>> es =
                {{0, Axis::X}, {0, Axis::Y}, {0, Axis::Z},
                 {Axis::X, Axis::X|Axis::Y}, {Axis::X, Axis::X|Axis::Z},
                 {Axis::Y, Axis::Y|Axis::X}, {Axis::Y, Axis::Y|Axis::Z},
                 {Axis::X|Axis::Y, Axis::X|Axis::Y|Axis::Z},
                 {Axis::Z, Axis::Z|Axis::X}, {Axis::Z, Axis::Z|Axis::Y},
                 {Axis::Z|Axis::X, Axis::Z|Axis::X|Axis::Y},
                 {Axis::Z|Axis::Y, Axis::Z|Axis::Y|Axis::X}};
            for (auto e : es)
                m->line(t->cornerPos(e.first).template cast<float>(),
                        t->cornerPos(e.second).template cast<float>());
        }
#endif
        return m;
    }
}

void Mesh::line(const Eigen::Vector3f& a, const Eigen::Vector3f& b)
{
    uint32_t a_ = verts.size();
    verts.push_back(a);
    uint32_t b_ = verts.size();
    verts.push_back(b);

    branes.push_back({a_, a_, b_});
}

////////////////////////////////////////////////////////////////////////////////

bool Mesh::saveSTL(const std::string& filename,
                   const std::list<const Mesh*>& meshes)
{
    if (!boost::algorithm::iends_with(filename, ".stl"))
    {
        std::cerr << "Mesh::saveSTL: filename \"" << filename
                  << "\" does not end in .stl" << std::endl;
    }
    std::ofstream file;
    file.open(filename, std::ios::out | std::ios::binary);
    if (!file.is_open())
    {
        std::cout << "Mesh::saveSTL: could not open " << filename
                  << std::endl;
        return false;
    }

    // File header (giving human-readable info about file type)
    std::string header = "This is a binary STL exported from libfive.";
    file.write(header.c_str(), header.length());

    // Pad the rest of the header to 80 bytes
    for (int i=header.length(); i < 80; ++i)
    {
        file.put(' ');
    }

    // Write the triangle count to the file
    uint32_t num = std::accumulate(meshes.begin(), meshes.end(), (uint32_t)0,
            [](uint32_t i, const Mesh* m){ return i + m->branes.size(); });
    file.write(reinterpret_cast<char*>(&num), sizeof(num));

    for (const auto& m : meshes)
    {
        for (const auto& t : m->branes)
        {
            // Write out the normal vector for this face (all zeros)
            float norm[3] = {0, 0, 0};
            file.write(reinterpret_cast<char*>(&norm), sizeof(norm));

            // Iterate over vertices (which are indices into the verts list)
            for (unsigned i=0; i < 3; ++i)
            {
                auto v = m->verts[t[i]];
                float vert[3] = {v.x(), v.y(), v.z()};
                file.write(reinterpret_cast<char*>(&vert), sizeof(vert));
            }

            // Write out this face's attribute short
            uint16_t attrib = 0;
            file.write(reinterpret_cast<char*>(&attrib), sizeof(attrib));
        }
    }

    return true;
}

bool Mesh::saveSTL(const std::string& filename)
{
    return saveSTL(filename, {this});
}

}   // namespace Kernel
