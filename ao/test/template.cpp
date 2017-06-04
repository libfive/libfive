#include "catch.hpp"

#include "ao/tree/template.hpp"

using namespace Kernel;

TEST_CASE("Template::serialize")
{
    SECTION("With a name")
    {
        auto a = Template(min(Tree::X(), Tree::Y()));
        a.name = "hi";
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', 'h', 'i', '"', '"', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::MIN, 1, 0, 0, 0, 0, 0, 0, 0};
        REQUIRE(out == expected);
    }

    SECTION("String escaping")
    {
        auto a = Template(min(Tree::X(), Tree::Y()));
        a.name = "hi";
        a.doc = "\"\\";
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', 'h', 'i', '"', '"', '\\', '"', '\\', '\\', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::MIN, 1, 0, 0, 0, 0, 0, 0, 0};
        REQUIRE(out == expected);
    }
}
