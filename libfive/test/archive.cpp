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
#include "libfive/tree/serializer.hpp"
#include "libfive/tree/deserializer.hpp"

#include "libfive/tree/oracle_clause.hpp"
#include "libfive/eval/oracle.hpp"

using namespace Kernel;

class ST : public OracleClause
{
public:
    std::string name() const override { return "ST"; }
    std::unique_ptr<Oracle> getOracle() const override { return nullptr; }

    bool serialize(Serializer& out) const
    {
        out.serializeString("hi");
        return true;
    }

    static std::unique_ptr<const OracleClause> deserialize(Deserializer& in)
    {
        auto out = in.deserializeString();
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
        std::stringstream out;
        a.serialize(out);
        std::string expected =
            {'T', '"', 'h', 'i', '"', '"', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }

    SECTION("Multiple independent trees")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()));
        a.addShape(max(Tree::X(), Tree::Y()));

        std::stringstream out;
        a.serialize(out);
        std::string expected =
            {'T', '"', '"', '"', '"',
                Opcode::VAR_X, Opcode::VAR_Y,
                Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF,
             'T', '"', '"', '"', '"',
                Opcode::OP_MAX, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }

    SECTION("Trees with already-stored root")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()));
        a.addShape(Tree::X());

        std::stringstream out;
        a.serialize(out);

        std::string expected =
            {'T', '"', '"', '"', '"',
                Opcode::VAR_X, Opcode::VAR_Y,
                Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF,
             't', '"', '"', '"', '"',
                0, 0, 0, 0, (char)0xFF};
        REQUIRE(out.str() == expected);
    }

    SECTION("String escaping")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()), "hi", "\"\\");

        std::stringstream out;
        a.serialize(out);

        std::string expected =
            {'T', '"', 'h', 'i', '"', '"', '\\', '"', '\\', '\\', '"', Opcode::VAR_X, Opcode::VAR_Y, Opcode::OP_MIN, 1, 0, 0, 0, 0, 0, 0, 0, (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }

    SECTION("With an oracle")
    {
        auto a = Archive(Tree(std::unique_ptr<OracleClause>(
                        new ST())));

        std::stringstream out;
        a.serialize(out);

        std::string expected =
            {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"', (char)0xFF, (char)0xFF};
        REQUIRE(out.str() == expected);
    }
}

TEST_CASE("Archive::deserialize")
{
    SECTION("Oracle")
    {
        std::string s =
                {'T', '"', '"', '"', '"', Opcode::ORACLE, '"', 'S', 'T', '"', '"', 'h', 'i', '"'};

        std::stringstream in(s);
        auto t = Archive::deserialize(in).shapes.front();
        REQUIRE(t.tree.id() != nullptr);
        REQUIRE(t.tree->op == Opcode::ORACLE);
        REQUIRE(dynamic_cast<const ST*>(t.tree->oracle.get())
                != nullptr);
    }

    SECTION("Complicated")
    {
        auto a = Archive();
        a.addShape(min(Tree::X(), Tree::Y()), "I'm a tree!",
                "With a \"docstring\"");
        a.addShape(Tree::X(), "HELLO");
        a.addShape(Tree::Z() + Tree::X() + min(Tree::X(), Tree::Y()));

        std::stringstream out;
        a.serialize(out);

        std::stringstream in(out.str());
        auto b = Archive::deserialize(in);
        REQUIRE(b.shapes.size() == a.shapes.size());

        auto a_itr = a.shapes.begin();
        auto b_itr = b.shapes.begin();
        while(a_itr != a.shapes.end())
        {
            REQUIRE(a_itr->tree == b_itr->tree);
            REQUIRE(a_itr->name == b_itr->name);
            REQUIRE(a_itr->doc == b_itr->doc);

            ++a_itr;
            ++b_itr;
        }
    }
}
