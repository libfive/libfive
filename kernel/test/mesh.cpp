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
#include <catch/catch.hpp>

#include "ao/kernel/format/mesh.hpp"

TEST_CASE("Saving a mesh")
{
    Mesh m;
    m.verts.push_back({0,0,0});
    m.tris.push_back({0,0,0});

    std::stringstream ss;
    m.writeSTL(ss);
    REQUIRE(ss.str().size() ==
             80 // Header
           + sizeof(uint32_t) // Number of triangles
           + 3*sizeof(float)*4 // Normal and vertex position
           + sizeof(uint16_t));
}

TEST_CASE("Mesh normals")
{
    Mesh m;
    m.verts.push_back({0,0,0});
    m.verts.push_back({1,0,0});
    m.verts.push_back({0,1,0});

    m.tris.push_back({0, 1, 2});
    REQUIRE(m.norm(0) == glm::vec3(0, 0, 1));

    m.tris.push_back({0, 2, 1});
    REQUIRE(m.norm(1) == glm::vec3(0, 0, -1));
}
