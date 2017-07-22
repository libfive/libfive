#include <chrono>

#include "catch.hpp"

#include "ao/render/brep/mesh.hpp"
#include "ao/render/brep/region.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

/*
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
*/

TEST_CASE("Mesh::render (sphere)")
{
    Tree s = sphere(0.5);
    Region<3> r({-0.5, -0.5, -0.5}, {0.5, 0.5, 0.5});

    auto mesh = Mesh::render(s, r);
    mesh->saveSTL("/Users/mkeeter/Desktop/sphere.stl");
}

TEST_CASE("Mesh::render (cube)")
{
    auto cube = max(max(
                    max(-(Tree::X() + 1.5),
                          Tree::X() - 1.5),
                    max(-(Tree::Y() + 1.5),
                          Tree::Y() - 1.5)),
                    max(-(Tree::Z() + 1.5),
                          Tree::Z() - 1.5));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    auto mesh = Mesh::render(cube, r);
    mesh->saveSTL("/Users/mkeeter/Desktop/cube.stl");
}

TEST_CASE("Mesh::render (performance)")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    Tree sponge = menger(2);

    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    // Begin timekeeping
    start = std::chrono::system_clock::now();
    auto mesh = Mesh::render(sponge, r);
    end = std::chrono::system_clock::now();
    mesh->saveSTL("/Users/mkeeter/Desktop/out.stl");

    elapsed = end - start;

    auto elapsed_ms =
        std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

    std::string log = "\nMade sponge mesh in " +
           std::to_string(elapsed.count()) + " sec";
    WARN(log);
}

/*
TEST_CASE("Face count in rectangular prism")
{
    auto t = max(max(max(-Tree::X(), Tree::X() - 4),
                     max(-Tree::Y(), Tree::Y() - 1)),
                     max(-Tree::Z(), Tree::Z() - 0.25));
    auto m = Mesh::render(t, Region({-1, 5}, {-1, 2}, {-1, 1.25}, 8));
    REQUIRE(m->verts.size() == 8);
    REQUIRE(m->tris.size() == 12);
}
*/
