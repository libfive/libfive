#include <catch/catch.hpp>

#include "kernel/format/mesh.hpp"
#include "kernel/format/contours.hpp"

#include "kernel/render/region.hpp"
#include "kernel/eval/evaluator.hpp"

// Overloaded toString for glm::vec3
#include "util/glm.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Small sphere mesh")
{
    Tree t = sphere(0.5);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);

    auto m = Mesh::render(t, r);

    REQUIRE(m.tris.size() == 12);
}

TEST_CASE("Face normals")
{
    Tree axis[3] = {Tree::X(), Tree::Y(), Tree::Z()};
    glm::vec3 norm[3] = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 2);

    SECTION("Positive")
    {
        for (int i=0; i < 3; ++i)
        {
            auto m = Mesh::render(axis[i], r);
            for (unsigned j=0; j < m.tris.size(); ++j)
            {
                REQUIRE(m.norm(j) == norm[i]);
            }
        }
    }

    SECTION("Negative")
    {
        for (int i=0; i < 3; ++i)
        {
            Tree t(Tree(
                        Opcode::NEG,
                        Tree(Opcode::ADD, axis[i],
                            Tree(0.75))));
            auto m = Mesh::render(t, r);
            for (unsigned j=0; j < m.tris.size(); ++j)
            {
                REQUIRE(m.norm(j) == -norm[i]);
            }
        }
    }
}
