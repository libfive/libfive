#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_deriv_array.hpp"

using namespace Kernel;

TEST_CASE("DerivArrayEvaluator::deriv")
{
    SECTION("Every operator")
    {
        for (unsigned i=7; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            Tape tape(t);
            DerivArrayEvaluator e(tape);
            e.deriv({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("var + 2*X")
    {
        auto v = Tree::var();
        Tape t(v + 2 * Tree::X());
        DerivArrayEvaluator e(t);

        auto out = e.deriv({2, 0, 0});
        REQUIRE(out.col(0) == Eigen::Vector4f(2, 0, 0, 4));
    }
}

TEST_CASE("DerivArrayEvaluator::derivs")
{
    SECTION("X")
    {
        Tape t(Tree::X());
        DerivArrayEvaluator e(t);
        e.set({0, 0, 0}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);

        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 0, 0));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 0, 1));
    }

    SECTION("X + Z")
    {
        Tape t(Tree::X() + Tree::Z());
        DerivArrayEvaluator e(t);

        e.set({1, 1, 1}, 0);
        e.set({1, 2, 3}, 1);
        auto d = e.derivs(2);


        REQUIRE(d.col(0).matrix() == Eigen::Vector4f(1, 0, 1, 2));
        REQUIRE(d.col(1).matrix() == Eigen::Vector4f(1, 0, 1, 4));
    }
}
