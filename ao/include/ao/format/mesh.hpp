#pragma once

#include <vector>

#include <glm/vec3.hpp>

#include "ao/render/octree.hpp"
#include "ao/tree/tree.hpp"

namespace Kernel {

class Region;

struct Mesh
{
    /*
     *  Run dual contouring on the given evaluator, returning a Mesh
     *
     *  This involves sampling the evaluator on an octree (with QEF
     *  simplification to collapse leaf cells), then using DC to generate
     *  a triangle mesh.
     */
    static Mesh render(const Tree t, const Region& r);

    /*
     *  Saves the given mesh as a .stl file
     */
    void writeMeshToFile(std::string filename);
    void writeSTL(std::ostream& out);
    void writeOBJ(std::ostream& out);

    /*
     *  Returns the unit-length normal of the i'th triangle
     */
    glm::vec3 norm(unsigned i) const;

    /*  Vertices in 3D space  */
    std::vector<glm::vec3> verts;

    /*  Triangles in the mesh (as indices in verts list)  */
    std::vector<glm::uvec3> tris;
};

}   // namespace Kernel
