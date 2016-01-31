#include <catch/catch.hpp>

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Variable evaluation")
{
    Store s;
    Tree t(&s, s.X());
    Evaluator e(&t);

    REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 1.0);
}

TEST_CASE("Float evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));
    Evaluator e(&t);

    REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 2.0);
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
