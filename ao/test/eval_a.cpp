#include <Eigen/Geometry>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_a.hpp"

using namespace Kernel;

TEST_CASE("ArrayEvaluator::eval")
{
    SECTION("X")
    {
        Tape t(Tree::X());
        ArrayEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 1.0);
    }

    SECTION("Y")
    {
        Tape t(Tree::Y());
        ArrayEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("Constant")
    {
        Tape t(Tree(3.14));
        ArrayEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("Secondary variable")
    {
        auto v = Tree::var();
        Tape t(v);
        ArrayEvaluator e(t, {{v.id(), 3.14}});
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(3.14));
    }

    SECTION("X + 1")
    {
        Tape t(Tree::X() + 1);
        ArrayEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 2.0);
    }

    SECTION("X + Z")
    {
        Tape t(Tree::X() + Tree::Z());
        ArrayEvaluator e(t);
        REQUIRE(e.eval({1.0, 2.0, 3.0}) == 4.0);
    }

    SECTION("Every operation")
    {
        for (unsigned i=7; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            Tape p(t);
            ArrayEvaluator e(p);
            e.eval({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }
}

TEST_CASE("ArrayEvaluator::setVar")
{
    // Deliberately construct out of order
    auto a = Tree::var();
    auto c = Tree::var();
    auto b = Tree::var();

    Tape t(a*1 + b*2 + c*3);
    ArrayEvaluator e(t, {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});
    REQUIRE(e.eval({0, 0, 0}) == Approx(34));

    e.setVar(a.id(), 5);
    REQUIRE(e.eval({0, 0, 0}) == Approx(36));
    e.setVar(b.id(), 0);
    REQUIRE(e.eval({0, 0, 0}) == Approx(26));
    e.setVar(c.id(), 10);
    REQUIRE(e.eval({0, 0, 0}) == Approx(35));
}

TEST_CASE("ArrayEvaluator::evalAndPush")
{
    Tape t(min(Tree::X(), Tree::Y()));
    ArrayEvaluator e(t);

    e.evalAndPush({-1, 0, 0}); // specialize to just "X"
    REQUIRE(e.eval({-2, 0, 0}) == -2);
    REQUIRE(e.eval({4, 0, 0}) == 4);
    REQUIRE(e.eval({4, 5, 0}) == 4);
    REQUIRE(e.eval({10, 5, 0}) == 10);

    e.pop();
    e.evalAndPush({0, -1, 0}); // specialize to just "Y"
    REQUIRE(e.eval({-2, 0, 0}) == 0);
    REQUIRE(e.eval({4, 0, 0}) == 0);
    REQUIRE(e.eval({4, 5, 0}) == 5);
    REQUIRE(e.eval({10, 5, 0}) == 5);
}
