#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_deriv.hpp"

using namespace Kernel;

TEST_CASE("DerivEvaluator::deriv")
{
    SECTION("Every operator")
    {
        for (unsigned i=7; i < Kernel::Opcode::LAST_OP; ++i)
        {
            auto op = (Kernel::Opcode::Opcode)i;
            Tree t = (Opcode::args(op) == 2 ? Tree(op, Tree::X(), Tree(5))
                                            : Tree(op, Tree::X()));
            Tape tape(t);
            DerivEvaluator e(tape);
            e.deriv({0, 0, 0});
            REQUIRE(true /* No crash! */ );
        }
    }

    SECTION("var + 2*X")
    {
        auto v = Tree::var();
        Tape t(v + 2 * Tree::X());
        DerivEvaluator e(t);

        auto out = e.deriv({2, 0, 0});
        REQUIRE(out == Eigen::Vector4f(2, 0, 0, 4));
    }
}
