#include <catch/catch.hpp>

#include <glm/gtx/string_cast.hpp>

#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

#include "ao/render/octree.hpp"
#include "ao/render/region.hpp"

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
        CAPTURE(glm::to_string(out->pos(i)));
        CAPTURE(glm::to_string(out->child(i)->pos(i)));
        REQUIRE(out->pos(i) == out->child(i)->pos(i));
    }
}
