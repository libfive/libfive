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

TEST_CASE("Tree::serialize")
{
    SECTION("Basic")
    {
        auto a = min(Tree::X(), Tree::Y());
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', '"', '"', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::MIN, 1, 0, 0, 0, 0, 0, 0, 0};
        REQUIRE(out == expected);
    }

    SECTION("With local references")
    {
        auto a = min(Tree::X(), Tree::X());
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', '"', '"', '"', Opcode::VAR_X, Opcode::MIN, 0, 0, 0, 0, 0, 0, 0, 0};
        REQUIRE(out == expected);
    }
}

TEST_CASE("Tree::deserialize")
{
    SECTION("Simple")
    {
        auto a = Tree::deserialize(min(Tree::X(), Tree::Y()).serialize());
        REQUIRE(a.id() != nullptr);
        REQUIRE(a->op == Opcode::MIN);
        REQUIRE(a->lhs->op == Opcode::VAR_X);
        REQUIRE(a->rhs->op == Opcode::VAR_Y);
    }
}
