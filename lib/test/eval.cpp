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
