#include <catch/catch.hpp>

#include "ao/kernel/render/dc.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

TEST_CASE("Small sphere mesh")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
                                   s.operation(OP_MUL, s.Z(), s.Z())),
               s.constant(0.5)));

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);

    auto m = DC::Render(&t, r);

    REQUIRE(m.tris.size() == 12);
}

TEST_CASE("Face counts")
{
    Store s;

    SECTION("Single level of recursion")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);

        for (Token* axis : {s.X(), s.Y(), s.Z()})
        {
            Tree t(&s, s.operation(OP_ADD, axis, s.constant(0.75)));
            auto m = DC::Render(&t, r);
            REQUIRE(m.tris.size() == 2);
        }
    }

    SECTION("Two levels of recursion")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 2);

        for (Token* axis : {s.X(), s.Y(), s.Z()})
        {
            Tree t(&s, s.operation(OP_ADD, axis, s.constant(0.75)));
            auto m = DC::Render(&t, r);
            REQUIRE(m.tris.size() == 18);
        }
    }
}
