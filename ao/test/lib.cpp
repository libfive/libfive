#include "catch.hpp"

#include "ao/tree/opcode.hpp"
#include "lib.h"

using namespace Kernel;

TEST_CASE("ao_opcode_enum")
{
    REQUIRE(ao_opcode_enum("min") == Opcode::MIN);
    REQUIRE(ao_opcode_enum("max") == Opcode::MAX);
}

TEST_CASE("ao_tree")
{
    auto x = ao_tree_x();
    auto y = ao_tree_y();
    auto sum = ao_tree_binary(Opcode::ADD, x, y);

    ao_tree_delete(x);
    ao_tree_delete(y);
    ao_tree_delete(sum);
}
