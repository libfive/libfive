#include <numeric>
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "ao/render/brep/mesh.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

template <Axis::Axis A, bool D>
void Mesh::load(const std::array<const XTree<3>*, 4>& ts)
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
        // Load either a patch-specific vertex (if this is a lowest-level,
        // potentially non-manifold cell) or the default vertex
        auto vi = ts[i]->level > 0
            ? 0
            : XTree<3>::mt->p[ts[i]->corner_mask][es[i]];
        assert(vi != -1);

        // Sanity-checking manifoldness of collapsed cells
        assert(ts[i]->level == 0 || ts[i]->vertex_count == 1);

        if (ts[i]->index[vi] == 0)
        {
            ts[i]->index[vi] = verts.size();

            verts.push_back(ts[i]->vert(vi).template cast<float>());
        }
        vs[i] = ts[i]->index[vi];
    }

    // Handle polarity-based windings
    if (!D)
    {
        std::swap(vs[1], vs[2]);
    }

    // Pick a triangulation that prevents triangles from folding back
    // on each other by checking normals.
    std::array<Eigen::Vector3f, 4> norms;
    for (unsigned i=0; i < norms.size(); ++i)
    {
        norms[i] = (verts[vs[(i + 3) % 4]] - verts[vs[i]]).cross
                   (verts[vs[(i + 1) % 4]] - verts[vs[i]]).normalized();
    }
    if (norms[0].dot(norms[3]) > norms[1].dot(norms[2]))
    {
        branes.push_back({vs[0], vs[1], vs[2]});
        branes.push_back({vs[2], vs[1], vs[3]});
    }
    else
    {
        branes.push_back({vs[0], vs[1], vs[3]});
        branes.push_back({vs[0], vs[3], vs[2]});
    }
}

////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Mesh> Mesh::render(const Tree t, const Region<3>& r,
                                   double min_feature, double max_err)
{
    std::atomic_bool cancel(false);
    std::map<Tree::Id, float> vars;
    return render(t, vars, r, min_feature, max_err, cancel);
}

std::unique_ptr<Mesh> Mesh::render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            std::atomic_bool& cancel)
{
    // Create the octree (multithreaded and cancellable)
    return mesh(XTree<3>::build(
            t, vars, r, min_feature, max_err, true, cancel), cancel);
}

std::unique_ptr<Mesh> Mesh::render(
        XTreeEvaluator* es,
        const Region<3>& r, double min_feature, double max_err,
        std::atomic_bool& cancel)
{
    return mesh(XTree<3>::build(es, r, min_feature, max_err, true, cancel),
                cancel);
}

std::unique_ptr<Mesh> Mesh::mesh(std::unique_ptr<const XTree<3>> xtree,
                                 std::atomic_bool& cancel)
{
    // Perform marching squares
    auto m = std::unique_ptr<Mesh>(new Mesh());

    if (cancel.load())
    {
        return nullptr;
    }
    else
    {
        Dual<3>::walk(xtree.get(), *m);

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

void Mesh::line(Eigen::Vector3f a, Eigen::Vector3f b)
{
    auto a_ = (unsigned)verts.size();
    verts.push_back(a);
    auto b_ = (unsigned)verts.size();
    verts.push_back(b);

    branes.push_back({a_, a_, b_});
}

////////////////////////////////////////////////////////////////////////////////

bool Mesh::saveSTL(const std::string& filename,
                   const std::list<const Mesh*>& meshes,
                   bool isBinary)
{
    if (!boost::algorithm::iends_with(filename, ".stl"))
    {
        std::cerr << "Mesh::saveSTL: filename \"" << filename
                  << "\" does not end in .stl" << std::endl;
        return false;
    }

    
    if (isBinary)
    {
      FILE * stl_file = fopen(filename.c_str(), "wb");
      if (stl_file == NULL)
      {
        std::cerr << "IOError: " << filename << " could not be opened for writing." << std::endl;
        return false;
      }


      std::string header = "This is a binary STL exported from Ao.";
      // Write unused 80-char header
      for (auto h : header)
      {
        fwrite(&h, sizeof(char), 1, stl_file);
      }

      // Write the rest of the 80-char header
      for (auto h = header.size(); h < 80; h++)
      {
        char o = '_';
        fwrite(&o, sizeof(char), 1, stl_file);
      }

      // Write number of triangles
      unsigned int num_tri = std::accumulate(meshes.begin(), meshes.end(), 0,
                                             [](unsigned int i, const Mesh* m)
      {
        return i + m->branes.size();
      });
      fwrite(&num_tri, sizeof(unsigned int), 1, stl_file);


      for (const auto& m : meshes)
      {
        for (const auto& t : m->branes)
        {
          // Write out the normal vector for this face (all zeros)
          std::vector<float> n(3, 0);
          fwrite(&n[0], sizeof(float), 3, stl_file);

          // Iterate over vertices (which are indices into the verts list)
          for (unsigned i = 0; i < 3; ++i)
          {
            auto vert = m->verts[t[i]];

            std::vector<float> v(3);
            v[0] = vert.x();
            v[1] = vert.y();
            v[2] = vert.z();
            fwrite(&v[0], sizeof(float), 3, stl_file);
          }

          // Write out this face's attribute short
          unsigned short att_count = 0;
          fwrite(&att_count, sizeof(unsigned short), 1, stl_file);
        }
      }

      fclose(stl_file);
    }
    else //ASCII
    {
      auto* stl_file = fopen(filename.c_str(), "w");
      if (stl_file == NULL)
      {
        std::cerr << "IOError: " << filename << " could not be opened for writing." << std::endl;
        return false;
      }
      fprintf(stl_file, "solid %s\n", filename.c_str());

      for (const auto& m : meshes)
      {
        for (const auto& t : m->branes)
        {
          // Write out the normal vector for this face (all zeros)
          fprintf(stl_file, "facet normal ");
          fprintf(stl_file, "0 0 0\n");

          fprintf(stl_file, "outer loop\n");

          // Iterate over vertices (which are indices into the verts list)
          for (unsigned i = 0; i < 3; ++i)
          {
            auto v = m->verts[t[i]];
           
            fprintf(stl_file, 
                    "vertex %e %e %e\n",
                    (float)v.x(),
                    (float)v.y(),
                    (float)v.z());
          }
          fprintf(stl_file, "endloop\n");
          fprintf(stl_file, "endfacet\n");
        }
      }
      fprintf(stl_file, "endsolid %s\n", filename.c_str());
      fclose(stl_file);
    }
    return true;
}

bool Mesh::saveSTL(const std::string& filename,
                   bool isBinary/* = true*/)
{
  return saveSTL(filename, { this }, isBinary);
}

}   // namespace Kernel
