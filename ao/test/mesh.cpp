#include <chrono>

#include "catch.hpp"

#include "ao/format/mesh.hpp"
#include "ao/render/region.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

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
    REQUIRE(m.norm(0) == Eigen::Vector3f(0, 0, 1));

    m.tris.push_back({0, 2, 1});
    REQUIRE(m.norm(1) == Eigen::Vector3f(0, 0, -1));
}


TEST_CASE("Mesh performance")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    Tree sponge = menger(2);

    Region r({-2.5, 2.5}, {-2.5, 2.5}, {-2.5, 2.5}, 25);

    // Begin timekeeping
    start = std::chrono::system_clock::now();
    auto mesh = Mesh::render(sponge, r);
    end = std::chrono::system_clock::now();

    elapsed = end - start;

    auto elapsed_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

    std::string log = "\nMade sponge mesh in " +
           std::to_string(elapsed.count()) + " sec";
    WARN(log);
}
