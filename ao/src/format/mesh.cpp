#include <fstream>
#include <string>
#include <iostream>
#include <iomanip>
#include <boost/algorithm/string/predicate.hpp>

#include "ao/format/mesh.hpp"

namespace Kernel {

void Mesh::writeMeshToFile(std::string filename)
{
    std::ofstream out;
    out.open(filename, std::ios::out);

    if (boost::algorithm::ends_with(filename, ".stl")) writeSTL(out);
    else if (boost::algorithm::ends_with(filename, ".obj")) writeOBJ(out);
    else std::cerr << "Mesh::writeMeshToStream -- unknown file extension";
}

void Mesh::writeSTL(std::ostream& out)
{
    // File header (giving human-readable info about file type)
    std::string header = "This is a binary STL exported from Ao.";
    out.write(header.c_str(), header.length());

    // Pad the rest of the header to 80 bytes
    for (int i=header.length(); i < 80; ++i)
    {
        out.put(' ');
    }

    // Write the number of triangles
    uint32_t num = tris.size();
    out.write(reinterpret_cast<char*>(&num), sizeof(num));

    for (auto t : tris)
    {
        // Write out the normal vector for this face (all zeros)
        float norm[3] = {0, 0, 0};
        out.write(reinterpret_cast<char*>(&norm), sizeof(norm));

        // Iterate over vertices (which are indices into the verts list
        for (auto i : {t[0], t[1], t[2]})
        {
            auto v = verts[i];
            float vert[3] = {v.x(), v.y(), v.z()};
            out.write(reinterpret_cast<char*>(&vert), sizeof(vert));
        }

        // Write out this face's attribute short
        uint16_t attrib = 0;
        out.write(reinterpret_cast<char*>(&attrib), sizeof(attrib));
    }
}

std::string formatFloat(double n) {
    // convert n to a string, but limit the number of digits after
    // the decimal place to 6.  Trim off trailing zeros and perhaps decimal.
    std::stringstream ss;
    ss << std::fixed << std::setprecision(6) << n;
    std::string s = ss.str();
    s.erase(s.find_last_not_of('0') + 1, std::string::npos);
    s.erase(s.find_last_not_of('.') + 1, std::string::npos);
    return s;
}

void Mesh::writeOBJ(std::ostream& out)
{
    for (auto v : verts)
    {
        out << "v ";
        out << formatFloat(v[0]) << " ";
        out << formatFloat(v[1]) << " ";
        out << formatFloat(v[2]) << "\n";
    }
    out << "\n";

    for (auto t : tris)
    {
        out << "f ";
        out << (t[0] + 1) << " ";
        out << (t[1] + 1) << " ";
        out << (t[2] + 1) << "\n";
    }
}

Eigen::Vector3f Mesh::norm(unsigned i) const
{
    assert(i < tris.size());

    const auto a = verts[tris[i][1]] - verts[tris[i][0]];
    const auto b = verts[tris[i][2]] - verts[tris[i][0]];

    return a.cross(b).normalized();
}

}   // namespace Kernel
