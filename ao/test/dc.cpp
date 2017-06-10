#include "catch.hpp"

#include "ao/format/mesh.hpp"
#include "ao/format/contours.hpp"

#include "ao/render/region.hpp"
#include "ao/eval/evaluator.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Small sphere mesh")
{
    Tree t = sphere(0.5);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);

    auto m = Mesh::render(t, r);

    REQUIRE(m->tris.size() == 12);
}

TEST_CASE("Face normals")
{
    Tree axis[3] = {Tree::X(), Tree::Y(), Tree::Z()};
    Eigen::Vector3f norm[3] = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 2);

    SECTION("Positive")
    {
        for (int i=0; i < 3; ++i)
        {
            auto m = Mesh::render(axis[i], r);
            for (unsigned j=0; j < m->tris.size(); ++j)
            {
                REQUIRE(m->norm(j) == norm[i]);
            }
        }
    }

    SECTION("Negative")
    {
        for (int i=0; i < 3; ++i)
        {
            Tree t(Tree(Opcode::NEG,
                        Tree(Opcode::ADD, axis[i],
                            Tree(0.75))));
            auto m = Mesh::render(t, r);
            for (unsigned j=0; j < m->tris.size(); ++j)
            {
                REQUIRE(m->norm(j) == -norm[i]);
            }
        }
    }
}
