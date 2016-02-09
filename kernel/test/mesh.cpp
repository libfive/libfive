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
