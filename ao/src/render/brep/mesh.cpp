#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "ao/render/brep/mesh.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

std::unique_ptr<Mesh> Mesh::render(const Tree t, const Region<3>& r)
{
    std::unique_ptr<Evaluator> eval(new Evaluator(t));

    // Create the quadtree on the scaffold
    auto xtree = XTree<3>(eval.get(), r);

    // Perform marching squares
    auto m = std::unique_ptr<Mesh>(new Mesh());
    Dual<3>::walk(&xtree, *m);
    return m;
}

void Mesh::operator()(const std::array<const XTree<3>*, 4>& ts)
{
    uint32_t vs[4];
    for (unsigned i=0; i < ts.size(); ++i)
    {
        if (ts[i]->index == 0)
        {
            ts[i]->index = verts.size();
            verts.push_back(ts[i]->vert.template cast<float>());
        }
        vs[i] = ts[i]->index;
    }

    // Pick a triangulation that prevents triangles from folding back
    // on each other by checking normals.
    std::array<Eigen::Vector3d, 4> norms;
    for (unsigned i=0; i < norms.size(); ++i)
    {
        norms[i] = (ts[(i + 3) % 4]->vert - ts[i]->vert).cross
                   (ts[(i + 1) % 4]->vert - ts[i]->vert).normalized();
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

bool Mesh::saveSTL(const std::string& filename)
{
    if (!boost::algorithm::iends_with(filename, ".stl"))
    {
        std::cerr << "Mesh::saveSTL: filename \"" << filename
                  << "\" does not end in .stl" << std::endl;
    }
    std::ofstream file;
    file.open(filename, std::ios::out);
    if (!file.is_open())
    {
        std::cout << "Mesh::saveSTL: could not open " << filename
                  << std::endl;
        return false;
    }

    // File header (giving human-readable info about file type)
    std::string header = "This is a binary STL exported from Ao.";
    file.write(header.c_str(), header.length());

    // Pad the rest of the header to 80 bytes
    for (int i=header.length(); i < 80; ++i)
    {
        file.put(' ');
    }

    // Write the number of triangles
    uint32_t num = branes.size();
    file.write(reinterpret_cast<char*>(&num), sizeof(num));

    for (const auto& t : branes)
    {
        // Write out the normal vector for this face (all zeros)
        float norm[3] = {0, 0, 0};
        file.write(reinterpret_cast<char*>(&norm), sizeof(norm));

        // Iterate over vertices (which are indices into the verts list)
        for (unsigned i=0; i < 3; ++i)
        {
            auto v = verts[t[i]];
            float vert[3] = {v.x(), v.y(), v.z()};
            file.write(reinterpret_cast<char*>(&vert), sizeof(vert));
        }

        // Write out this face's attribute short
        uint16_t attrib = 0;
        file.write(reinterpret_cast<char*>(&attrib), sizeof(attrib));
    }
    return true;
}

}   // namespace Kernel
