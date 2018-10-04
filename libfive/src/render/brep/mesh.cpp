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

#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/xtree_pool.hpp"
#include "libfive/render/brep/dual.hpp"

#define TRIANGLE_FAN_MESHING 1

namespace Kernel {

const float Mesh::MAX_PROGRESS = 3.0f;

template <Axis::Axis A, bool D>
void Mesh::load(const std::array<const XTree<3>*, 4>& ts, unsigned index)
{
    int es[4];
    {   // Unpack edge vertex pairs into edge indices
        auto q = Axis::Q(A);
        auto r = Axis::R(A);
        std::vector<std::pair<unsigned, unsigned>> ev = {
            {q|r, q|r|A},
            {r, r|A},
            {q, q|A},
            {0, A}};
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

    // Handle polarity-based windings
    if (!D)
    {
        std::swap(vs[1], vs[2]);
    }

#if TRIANGLE_FAN_MESHING
    // Get the intersection vertex.  It should be the same
    // position regardless of which vertex in the array we use
    // (they should differ only by derivative).
    assert(ts[index]->intersection(es[index]).get() != nullptr);
    auto& intersectVec = *ts[index]->intersection(es[index]);
    assert(!intersectVec.empty());
    auto intersectPos = intersectVec[0].pos;
    for (auto& inter : intersectVec)
    {
        (void)inter;
        assert(inter.pos == intersectPos);
    }

    uint32_t vCenter = verts.size();
    verts.push_back(intersectPos.template cast<float>());

    // Rather than triangulating our quad into two triangles, we triangulate
    // into four triangles, sharing a vertex at our computed intersection.  
    // This gives us greater accuracy at no extra computation cost, and (more
    // importantly) is the first step to ensuring that every triangle is
    // contained in the cells that generated it, greatly reducing the 
    // opportunities for self-intersection.
    branes.push_back({ vs[0], vs[1], vCenter });
    branes.push_back({ vs[1], vs[3], vCenter });
    branes.push_back({ vs[3], vs[2], vCenter });
    branes.push_back({ vs[2], vs[0], vCenter });

#else
    (void)index; // This variable is unused in non-fan meshing

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

std::unique_ptr<Mesh> Mesh::mesh(const XTree<3>::Root& xtree,
                                 std::atomic_bool& cancel,
                                 ProgressCallback progress_callback)
{
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
