#include <catch/catch.hpp>

#include "store.hpp"
#include "tree.hpp"

TEST_CASE("Double evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));

    t.setMode<double>();
    REQUIRE(t.eval(1.0, 2.0, 3.0) == 2.0);
}

TEST_CASE("Interval evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));

    t.setMode<Interval>();

    Interval arg(1, 2);
    auto out = t.eval(arg, arg, arg);

    REQUIRE(out.lower() == 2.0);
    REQUIRE(out.upper() == 3.0);
}

TEST_CASE("List of doubles evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.constant(1)));

    t.setMode<double>();

    std::vector<double> args({1, 2, 3, 4, 5});
    std::vector<double>  out({2, 3, 4, 5, 6});

    REQUIRE(t.eval(args, args, args) == out);
}
