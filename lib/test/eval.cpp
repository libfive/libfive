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

TEST_CASE("Condition statement double")
{
    Store s;
    Tree t(&s, s.operation(COND_LZ, s.constant(1), s.constant(2), s.X()));

    REQUIRE(t.eval(1.0, 0.0, 0.0) == 2);
    REQUIRE(t.eval(0.0, 0.0, 0.0) == 2);
    REQUIRE(t.eval(-1.0, 0.0, 0.0) == 1);
}

TEST_CASE("Condition statement interval")
{
    Store s;
    Tree t(&s, s.operation(COND_LZ, s.constant(1), s.constant(2), s.X()));

    SECTION("Above")
    {
        Interval i(0.5, 1.0);
        Interval out = t.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out == Interval(2));
    }

    SECTION("Below")
    {
        Interval i(-1, -0.5);
        Interval out = t.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out == Interval(1));
    }

    SECTION("Split")
    {
        Interval i(-1, 1);
        Interval out = t.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out.lower() == 1);
        REQUIRE(out.upper() == 2);
    }
}
