#include <Eigen/Geometry>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_i.hpp"
#include "ao/eval/eval_a.hpp"
#include "ao/render/brep/region.hpp"

using namespace Kernel;

TEST_CASE("IntervalEvaluator::eval")
{
    SECTION("Basic math")
    {
        Tape t(Tree::X() + 1);
        IntervalEvaluator e(t);

        auto out = e.eval({1,1,1}, {2,2,2});

        REQUIRE(out.lower() == 2.0);
        REQUIRE(out.upper() == 3.0);
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            Tape p(t);
            IntervalEvaluator e(p);
            e.eval({0, 0, 0}, {1, 1, 1});
            REQUIRE(true /* No crash! */ );
        }
    }
}

TEST_CASE("IntervalEvaluator::evalAndPush")
{
    SECTION("Basic")
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

    SECTION("With NaNs")
    {
        auto x = Tree::X();
        auto y = Tree::Y();
        auto r = sqrt(x*x + y*y);
        auto t = atan(y / x);
        auto tree = max(-Tree::Z(), r - (2 + t));

        Region<3> ra({-0.3125, -3.4375, -0.3125}, {0, -3.125, 0});
        Region<3> rb({-0.3125, -3.4375, 0}, {0, -3.125, 0.3125});
        Eigen::Vector3f target(0, -3.4375, 0);

        // Initial sanity-checking
        REQUIRE(ra.contains(target.template cast<double>()));
        REQUIRE(rb.contains(target.template cast<double>()));

        Tape tape(tree);
        IntervalEvaluator eval(tape);
        ArrayEvaluator eval_(tape);

        auto ia = eval.evalAndPush(ra.lower.template cast<float>(),
                                   ra.upper.template cast<float>());
        CAPTURE(ia.lower());
        CAPTURE(ia.upper());
        CAPTURE(tape.utilization());
        auto ea = eval_.eval(target);
        eval.pop();

        auto ib = eval.evalAndPush(rb.lower.template cast<float>(),
                                   rb.upper.template cast<float>());
        CAPTURE(ib.lower());
        CAPTURE(ib.upper());
        CAPTURE(tape.utilization());
        auto eb = eval_.eval(target);
        eval.pop();

        REQUIRE(ea == eb);
    }
}
