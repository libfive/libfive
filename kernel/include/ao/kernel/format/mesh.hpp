/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <vector>

#include <glm/vec3.hpp>

#include "ao/kernel/render/octree.hpp"

class Tree;
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
    static Mesh Render(Tree* t, const Region& r);

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
