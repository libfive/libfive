#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/eval_jacobian.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("JacobianEvaluator::gradient")
{
    SECTION("constant + variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Tape>(v + 1.0);
        JacobianEvaluator e(t, {{v.id(), 3.14}});

        REQUIRE(e.eval({1.0, 2.0, 3.0}) == Approx(4.14));
        auto g = e.gradient({1, 2, 3});
        REQUIRE(g.size() == 1);
        REQUIRE(g.count(v.id()) == 1);
        REQUIRE(g.at(v.id()) == Approx(1));
    }

    SECTION("x * variable")
    {
        auto v = Tree::var();
        auto t = std::make_shared<Tape>(Tree::X() * v);
        JacobianEvaluator e(t, {{v.id(), 1}});
        {
            auto g = e.gradient({2, 0, 0});
            REQUIRE(g.size() == 1);
            REQUIRE(g.at(v.id()) == Approx(2));
        }
        {
            auto g = e.gradient({3, 0, 0});
            REQUIRE(g.at(v.id()) == Approx(3));
        }
    }

    SECTION("Multiple variables")
    {
        // Deliberately construct out of order
        auto a = Tree::var();
        auto c = Tree::var();
        auto b = Tree::var();

        auto t = std::make_shared<Tape>(a*1 + b*2 + c*3);
        JacobianEvaluator e(t,
                {{a.id(), 3}, {c.id(), 7}, {b.id(), 5}});

        REQUIRE(e.eval({0, 0, 0}) == Approx(34));

        auto g = e.gradient({0, 0, 0});
        REQUIRE(g.at(a.id()) == Approx(1.0f));
        REQUIRE(g.at(b.id()) == Approx(2.0f));
        REQUIRE(g.at(c.id()) == Approx(3.0f));
    }
}
