#include "catch.hpp"

#include "ao/tree/tree.hpp"

using namespace Kernel;

TEST_CASE("Joining two trees")
{
    auto t = Tree::X() + 1;

    REQUIRE(t->op == Opcode::ADD);
    REQUIRE(t->lhs->op == Opcode::VAR_X);
    REQUIRE(t->rhs->op == Opcode::CONST);
    REQUIRE(t->rhs->value == 1);
}

TEST_CASE("Tree::flags")
{
    auto a = Tree::var();
    REQUIRE(a->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto b = Tree::var();
    REQUIRE(b->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto c = a + b;
    REQUIRE(c->flags == Tree::FLAG_LOCATION_AGNOSTIC);

    auto d = c + Tree::X();
    REQUIRE(d->flags == 0);
}
