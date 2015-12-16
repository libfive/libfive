#include <catch/catch.hpp>

#include "ao/core/store.hpp"
#include "ao/core/token.hpp"

TEST_CASE("Constructing a simple shape")
{
    Store s;
    Token* out = s.operation(OP_ADD, s.X(), s.constant(1));
    REQUIRE(out != nullptr);
}

TEST_CASE("Deduplication of variables")
{
    Store s;

    Token* xa = s.X();
    Token* xb = s.X();
    REQUIRE(xa == xb);

    Token* ya = s.Y();
    REQUIRE(xa != ya);
}

TEST_CASE("Deduplication of constants")
{
    Store s;

    Token* ca = s.constant(3.14);
    Token* cb = s.constant(3.14);
    REQUIRE(ca == cb);

    Token* cc = s.constant(4);
    REQUIRE(ca != cc);
}

TEST_CASE("Deduplication of operations")
{
    Store s;

    Token* oa = s.operation(OP_ADD, s.X(), s.constant(1));
    Token* ob = s.operation(OP_ADD, s.X(), s.constant(1));
    REQUIRE(oa == ob);

    Token* oc = s.operation(OP_ADD, s.X(), s.constant(2));
    REQUIRE(oa != oc);
}

TEST_CASE("Found flag propagation")
{
    Store s;

    Token* oa = s.operation(OP_ADD, s.X(), s.constant(1));
    Token* ob = s.operation(OP_MUL, s.Y(), s.constant(1));

    s.markFound(oa);

    REQUIRE(oa->isFound());
    REQUIRE(s.X()->isFound());
    REQUIRE(s.constant(1)->isFound());

    REQUIRE(!ob->isFound());
    REQUIRE(!s.Y()->isFound());
}
