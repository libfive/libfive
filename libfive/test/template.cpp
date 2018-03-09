/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "catch.hpp"

#include "libfive/tree/template.hpp"
#include "libfive/tree/oracle_clause.hpp"
#include "libfive/eval/oracle.hpp"

using namespace Kernel;

class ST : public OracleClause
{
public:
    std::string name() const override { return "ST"; }
    std::unique_ptr<Oracle> getOracle() const override { return nullptr; }

    bool serialize(std::vector<uint8_t>& data) const
    {
        Template::serializeString("hi", data);
        return true;
    }

    static std::unique_ptr<const OracleClause> deserialize(
            const uint8_t*& pos, const uint8_t* end)
    {
        auto out = Template::deserializeString(pos, end);
        if (out != "hi")
        {
            return nullptr;
        }
        return std::unique_ptr<const OracleClause>(new ST());
    }
};
REGISTER_ORACLE_CLAUSE(ST);

////////////////////////////////////////////////////////////////////////////////

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

    SECTION("With an oracle")
    {
        auto a = Template(Tree(std::unique_ptr<OracleClause>(
                        new ST())));
        a.name = "";
        a.doc = "";
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"'};
        REQUIRE(out == expected);
    }
}

TEST_CASE("Template::deserialize")
{
    SECTION("Valid")
    {
        std::vector<uint8_t> in =
                {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"'};
        auto t = Template::deserialize(in);
        REQUIRE(t.tree.id() != nullptr);
        REQUIRE(t.tree->op == Opcode::ORACLE);
        REQUIRE(dynamic_cast<const ST*>(t.tree->oracle.get())
                != nullptr);
    }
}
