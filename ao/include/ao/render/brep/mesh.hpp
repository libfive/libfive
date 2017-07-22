#pragma once

#include "ao/tree/tree.hpp"
#include "ao/render/brep/region.hpp"

namespace Kernel {

class Mesh {
public:
    static std::unique_ptr<Mesh> render(const Tree t, const Region<3>& r,
                                        const float max_err=1e-6);

    bool saveSTL(const std::string& filename);

    /*  Flat array of vertex positions  */
    std::vector<Eigen::Vector3f> verts;
    /*  Triangles, as a list of indices into verts array */
    std::vector<std::array<uint32_t, 3>> tris;

protected:
    /*
     *  The Mesh constructor expects to move arrays from a TetMarcher
     *  or similar to avoid copying large buffers around.
     */
    Mesh(std::vector<Eigen::Vector3f>&& verts,
         std::vector<std::array<uint32_t, 3>>&& tris)
        : verts(verts), tris(tris) {}
};

}   // namespace Kernel
