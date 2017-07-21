#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "ao/render/brep/mesh.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "ao/render/brep/mt.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

std::unique_ptr<Mesh> Mesh::render(const Tree t, const Region<3>& r,
                                   const float max_err)
{
    std::unique_ptr<Evaluator> eval(new Evaluator(t));

    // Create a padded scaffolding for the XTree
    const auto scaffold = Scaffold<3>(eval.get(), r, 4, true);

    // Create the quadtree on the scaffold
    auto xtree = XTree<3>(eval.get(), scaffold, max_err);

    // Perform marching squares
    TetMarcher ms(eval.get());
    Dual<3>::walk(xtree, ms);

    return std::unique_ptr<Mesh>(new Mesh(std::move(ms.verts),
                                          std::move(ms.tris)));
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
    uint32_t num = tris.size();
    file.write(reinterpret_cast<char*>(&num), sizeof(num));

    for (auto t : tris)
    {
        // Write out the normal vector for this face (all zeros)
        float norm[3] = {0, 0, 0};
        file.write(reinterpret_cast<char*>(&norm), sizeof(norm));

        // Iterate over vertices (which are indices into the verts list
        for (auto i : {std::get<0>(t), std::get<1>(t), std::get<2>(t)})
        {
            auto v = verts[i];
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
