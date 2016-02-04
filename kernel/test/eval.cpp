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

TEST_CASE("Push / pop behavior")
{
    Store s;
    Tree t(&s, s.operation(OP_MIN, s.X(), s.Y()));
    Evaluator e(&t);

    // Store -3 in the rhs's value
    REQUIRE(e.eval(1.0f, -3.0f, 0.0f) == -3);

    // Do an interval evaluation that will lead to disabling the rhs
    auto i = e.eval(Interval(-5, -4), Interval(8, 9), Interval(0, 0));
    REQUIRE(i.lower() == -5);
    REQUIRE(i.upper() == -4);

    // Push (which should disable the rhs of min
    e.push();

    // Check to make sure that the push disabled something
    REQUIRE(e.utilization() < 1);
    CAPTURE(e.utilization());

    // Require that the evaluation gets 1
    REQUIRE(e.eval(1.0f, 2.0f, 0.0f) == 1);
}
