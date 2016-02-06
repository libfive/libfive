#include <catch/catch.hpp>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"

// Overloaded toString for glm::vec3
#include "glm.hpp"

TEST_CASE("Octree coordinates")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
                                   s.operation(OP_MUL, s.Z(), s.Z())),
               s.constant(1)));

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);
    std::unique_ptr<Octree> out(Octree::Render(&t, r));
    REQUIRE(out->type == Octree::BRANCH);

    // Check that all child pointers are populated
    for (int i=0; i < 8; ++i)
    {
        CAPTURE(i);
        CAPTURE(out->child(i));
        REQUIRE(out->child(i) != nullptr);
    }

    // Check that Subregion::octsect and Octree::pos have consistent ordering
    for (int i=0; i < 8; ++i)
    {
        REQUIRE(out->pos(i) == out->child(i)->pos(i));
    }
}

TEST_CASE("Octree values")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
                                   s.operation(OP_MUL, s.Z(), s.Z())),
               s.constant(1)));

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);
    REQUIRE(r.X.values.size() == 2);

    std::unique_ptr<Octree> out(Octree::Render(&t, r));

    // Check that values and gradients are correct
    for (int i=0; i < 8; ++i)
    {
        REQUIRE(!out->corner(i));
    }
}

TEST_CASE("Octree intersections")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB, s.X(), s.constant(0.75)));

    SECTION("Leaf")
    {
        Region r({0, 1}, {0, 1}, {0, 1}, 1);
        std::unique_ptr<Octree> out(Octree::Render(&t, r));
        REQUIRE(out->type == Octree::LEAF);

        REQUIRE(out->intersections.size() == 4);
        for (auto i : out->intersections)
        {
            auto err = fabs(i.pos.x - 0.75);
            REQUIRE(err < 0.125);
        }
    }

    SECTION("Branch")
    {
        Region r({0, 1}, {0, 1}, {0, 1}, 2);
        std::unique_ptr<Octree> out(Octree::Render(&t, r));
        REQUIRE(out->type == Octree::BRANCH);

        REQUIRE(out->intersections.size() == 9);
        for (auto i : out->intersections)
        {
            auto err = fabs(i.pos.x - 0.75);
            REQUIRE(err < 0.125);
        }
    }
}
