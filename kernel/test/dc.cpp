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
               s.constant(1)));

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);

    auto m = DC::Render(&t, r);

    REQUIRE(m.tris.size() == 12);
}
