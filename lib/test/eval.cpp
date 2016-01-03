#include <catch/catch.hpp>

#include "ao/tree/store.hpp"
#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"

TEST_CASE("Variable evaluation")
{
    Store s;
    Tree t(&s, s.X());
    Evaluator e(&t);

    REQUIRE(e.eval(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Double evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));
    Evaluator e(&t);

    REQUIRE(e.eval(1.0, 2.0, 3.0) == 2.0);
}

TEST_CASE("Interval evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));
    Evaluator e(&t);

    Interval arg(1, 2);
    auto out = e.eval(arg, arg, arg);

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("Condition statement double")
{
    Store s;
    Tree t(&s, s.operation(COND_LZ, s.constant(1), s.constant(2), s.X()));
    Evaluator e(&t);

    REQUIRE(e.eval(1.0, 0.0, 0.0) == 2);
    REQUIRE(e.eval(0.0, 0.0, 0.0) == 2);
    REQUIRE(e.eval(-1.0, 0.0, 0.0) == 1);
}

TEST_CASE("Condition statement interval")
{
    Store s;
    Tree t(&s, s.operation(COND_LZ, s.constant(1), s.constant(2), s.X()));
    Evaluator e(&t);

    SECTION("Above")
    {
        Interval i(0.5, 1.0);
        Interval out = e.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out == Interval(2));
    }

    SECTION("Below")
    {
        Interval i(-1, -0.5);
        Interval out = e.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out == Interval(1));
    }

    SECTION("Split")
    {
        Interval i(-1, 1);
        Interval out = e.eval(i, i, i);
        CAPTURE(out.lower());
        CAPTURE(out.upper());
        REQUIRE(out.lower() == 1);
        REQUIRE(out.upper() == 2);
    }
}
