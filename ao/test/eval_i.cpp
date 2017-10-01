#include <Eigen/Geometry>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_i.hpp"

using namespace Kernel;

TEST_CASE("IntervalEvaluator::eval")
{
    Tape t(Tree::X() + 1);
    IntervalEvaluator e(t);

    auto out = e.eval({1,1,1}, {2,2,2});

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("IntervalEvaluator::evalAndPush")
{
    Tape t(min(Tree::X() + 1, Tree::Y() + 1));
    IntervalEvaluator e(t);

    // Store -3 in the rhs's value
    auto o = e.eval({1, -3, 0}, {1, -3, 0});
    REQUIRE(o.lower() == -2);
    REQUIRE(o.upper() == -2);

    // Do an interval evaluation that will lead to disabling the rhs
    // Pushing should disable the rhs of min
    auto i = e.evalAndPush({-5, 8, 0}, {-4, 9, 0});
    REQUIRE(i.lower() == -4);
    REQUIRE(i.upper() == -3);

    // Check to make sure that the push disabled something
    CAPTURE(t.utilization());
    REQUIRE(t.utilization() < 1);

    // Require that the evaluation gets 1
    o = e.eval({1, 2, 0}, {1, 2, 0});
    REQUIRE(o.lower() == 2);
    REQUIRE(o.upper() == 2);
}
