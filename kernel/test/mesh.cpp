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
#include <chrono>

#include <catch/catch.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/kernel/format/mesh.hpp"
#include "ao/kernel/render/region.hpp"
#include "ao/kernel/tree/tree.hpp"

#include "shapes.hpp"

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


TEST_CASE("Mesh performance")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    std::unique_ptr<Tree> sponge(menger(2));

    Region r({-2.5, 2.5}, {-2.5, 2.5}, {-2.5, 2.5}, 25);

    // Begin timekeeping
    start = std::chrono::system_clock::now();
    auto mesh = Mesh::Render(sponge.get(), r);
    end = std::chrono::system_clock::now();

    elapsed = end - start;

    auto elapsed_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

    std::string log = "\nMade sponge mesh in " +
           std::to_string(elapsed.count()) + " sec";
    WARN(log);
}
