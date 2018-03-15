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

#include "libfive/tree/archive.hpp"
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
        Archive::serializeString("hi", data);
        return true;
    }

    static std::unique_ptr<const OracleClause> deserialize(
            const uint8_t*& pos, const uint8_t* end)
    {
        auto out = Archive::deserializeString(pos, end);
        if (out != "hi")
        {
            return nullptr;
        }
        return std::unique_ptr<const OracleClause>(new ST());
    }
};
REGISTER_ORACLE_CLAUSE(ST);

////////////////////////////////////////////////////////////////////////////////

TEST_CASE("Archive::serialize")
{
    SECTION("With a name")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()), "hi");
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', 'h', 'i', '"', '"', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, 0xFF};
        REQUIRE(out == expected);
    }

    SECTION("String escaping")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()), "hi", "\"\\");
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', 'h', 'i', '"', '"', '\\', '"', '\\', '\\', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, 0xFF};
        REQUIRE(out == expected);
    }

    SECTION("With an oracle")
    {
        auto a = Archive(Tree(std::unique_ptr<OracleClause>(
                        new ST())));
        auto out = a.serialize();
        std::vector<uint8_t> expected =
            {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"', 0xFF};
        REQUIRE(out == expected);
    }
}

TEST_CASE("Archive::deserialize")
{
    SECTION("Oracle")
    {
        std::vector<uint8_t> in =
                {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"'};
        auto t = Archive::deserialize(in).shapes.front();
        REQUIRE(t.tree.id() != nullptr);
        REQUIRE(t.tree->op == Opcode::ORACLE);
        REQUIRE(dynamic_cast<const ST*>(t.tree->oracle.get())
                != nullptr);
    }
}
