#include <catch/catch.hpp>

#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"

TEST_CASE("Variable evaluation")
{
    Store s;
    Tree t(&s, s.X());

    REQUIRE(t.eval(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Double evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));

    REQUIRE(t.eval(1.0, 2.0, 3.0) == 2.0);
}

TEST_CASE("Interval evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));

    Interval arg(1, 2);
    auto out = t.eval(arg, arg, arg);

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("Condition statement")
{
    Store s;
    Tree t(&s, s.operation(COND_LZ, s.constant(1), s.constant(2), s.X()));

    REQUIRE(t.eval(1.0, 0.0, 0.0) == 2);
    REQUIRE(t.eval(0.0, 0.0, 0.0) == 2);
    REQUIRE(t.eval(-1.0, 0.0, 0.0) == 1);
}
