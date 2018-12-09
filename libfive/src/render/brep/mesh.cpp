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
#include "libfive/render/brep/dc/dc_pool.hpp"
#include "libfive/render/brep/dc/dc_mesher.hpp"
#include "libfive/render/brep/simplex/simplex_pool.hpp"
#include "libfive/render/brep/simplex/simplex_mesher.hpp"
#include "libfive/render/brep/dual.hpp"

#if LIBFIVE_TRIANGLE_FAN_MESHING
#include "libfive/render/brep/dc/intersection_aligner.hpp"
#endif

namespace Kernel {

const float Mesh::MAX_PROGRESS = 3.0f;

std::unique_ptr<Mesh> Mesh::render(const Tree t, const Region<3>& r,
                                   double min_feature, double max_err,
                                   bool multithread,
                                   ProgressCallback progress_callback,
                                   Algorithm alg)
{
    std::atomic_bool cancel(false);
    std::map<Tree::Id, float> vars;
    return render(t, vars, r, min_feature, max_err,
                  multithread ? 8 : 1, cancel, progress_callback, alg);
}

std::unique_ptr<Mesh> Mesh::render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            ProgressCallback progress_callback,
            Algorithm alg)
{
    std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
    es.reserve(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        es.emplace_back(XTreeEvaluator(t, vars));
    }

    return render(es.data(), r, min_feature, max_err, workers, cancel,
                  progress_callback, alg);
}

std::unique_ptr<Mesh> Mesh::render(
        XTreeEvaluator* es,
        const Region<3>& r, double min_feature, double max_err,
        int workers, std::atomic_bool& cancel,
        ProgressCallback progress_callback,
        Algorithm alg)
{
    std::unique_ptr<Mesh> out;
    if (alg == DUAL_CONTOURING)
    {
        auto t = DCPool<3>::build(
                es, r, min_feature, max_err, workers,
                cancel, progress_callback);

        if (cancel.load() || t.get() == nullptr) {
            return nullptr;
        }

#if LIBFIVE_TRIANGLE_FAN_MESHING
        // Make sure each intersection has the same object in all cells.
        Dual<3>::walk<IntersectionAligner>(t, 1, cancel, progress_callback);
        workers = 1;    // The fan walker isn't thread-safe
#endif

        // Perform marching squares
        out = Dual<3>::walk<DCMesher>(t, workers, cancel, progress_callback);

        // TODO: check for early return here again
        t.reset(progress_callback);
    }
    else if (alg == ISO_SIMPLEX)
    {
        auto t = SimplexTreePool<3>::build(
                es, r, min_feature, max_err, workers,
                cancel, progress_callback);

        if (cancel.load() || t.get() == nullptr) {
            return nullptr;
        }

        t->assignIndices();

        out = Dual<3>::walk_<SimplexMesher>(t, workers,
                cancel, progress_callback,
                [&](PerThreadBRep<3>& brep, int i) {
                    return SimplexMesher(brep, &es[i]);
                });
        t.reset(progress_callback);
    }

    return out;
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
