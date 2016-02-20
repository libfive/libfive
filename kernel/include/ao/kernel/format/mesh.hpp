/*
 *  Copyright (C) 2016 Matthew Keeter
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <vector>

#include <glm/vec3.hpp>

struct Mesh
{
    /*
     *  Saves the given mesh as a .stl file
     */
    void writeSTL(std::string filename);
    void writeSTL(std::ostream& out);

    /*
     *  Returns the unit-length normal of the i'th triangle
     */
    glm::vec3 norm(unsigned i) const;

    /*  Vertices in 3D space  */
    std::vector<glm::vec3> verts;

    /*  Triangles in the mesh (as indices in verts list)  */
    std::vector<glm::uvec3> tris;
};
