/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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
#include <iostream>
#include <map>

#include "libfive/tree/deserializer.hpp"
#include "libfive/tree/serializer.hpp"

namespace Kernel {

Archive Deserializer::run()
{
    Archive out;

    while (!in.eof())
    {
        out.shapes.push_back(deserializeShape());
    }
    return out;
}

Archive::Shape Deserializer::deserializeShape()
{
#define REQUIRE(cond) \
    if (!(cond)) \
    { \
        std::cerr << "Error: expected " << #cond \
                  << " at deserializer.cpp:" << __LINE__ << std::endl; \
    }
#define CHECK_POS() REQUIRE(!in.eof())

    CHECK_POS();
    char tag;
    in.get(tag);
    REQUIRE(tag == 'T' || tag == 't');
    CHECK_POS();

    Archive::Shape out;
    out.name = deserializeString();
    CHECK_POS();
    out.doc = deserializeString();

    if (tag == 't')
    {
        auto root = deserializeBytes<uint32_t>();
        out.tree = trees.at(root);
    }
    else
    {
        while (!in.eof())
        {
            CHECK_POS();

            // Check for END_OF_ITEM as a demarcation between the Tree
            // and the vars
            const uint8_t op_ = deserializeBytes<uint8_t>();
            if (op_ == Serializer::END_OF_ITEM)
            {
                break;
            }

            auto op = Opcode::Opcode(op_);

            REQUIRE(op > Opcode::INVALID);
            REQUIRE(op < Opcode::LAST_OP);
            CHECK_POS();

            auto args = Opcode::args(op);
            auto next = trees.size();
            if (op == Opcode::CONSTANT)
            {
                float v = deserializeBytes<float>();
                trees.insert({next, Tree(v)});
            }
            else if (op == Opcode::ORACLE)
            {
                std::string name = deserializeString();
                CHECK_POS();
                auto o = OracleClause::deserialize(name, *this);
                if (o.get() == nullptr)
                {
                    std::cerr
                        << "Archive::deserialize: failed to deserialize Oracle \""
                        << name << "\"" << std::endl;
                    return out;
                }
                trees.insert({next, Tree(std::move(o))});
            }
            else if (args == 2)
            {
                auto rhs = deserializeBytes<uint32_t>();
                auto lhs = deserializeBytes<uint32_t>();
                trees.insert({next, Tree(op, trees.at(lhs), trees.at(rhs))});
            }
            else if (args == 1)
            {
                auto lhs = deserializeBytes<uint32_t>();
                trees.insert({next, Tree(op, trees.at(lhs))});
            }
            else
            {
                trees.insert({next, Tree(op)});
            }
        }
        out.tree = trees.at(trees.size() - 1);
    }
    while (!in.eof())
    {
        CHECK_POS();

        // Check for END_OF_ITEM as a demarcation between Shapes
        const uint8_t op_ = deserializeBytes<uint8_t>();
        if (op_ == Serializer::END_OF_ITEM)
        {
            break;
        }

        auto varName = deserializeString();
        auto idx = deserializeBytes<uint32_t>();
        auto t = trees.find(idx);
        REQUIRE(t != trees.end());
        REQUIRE(out.vars.find(t->second.id()) == out.vars.end());
        out.vars[t->second.id()] = varName;
    }
    return out;
}

std::string Deserializer::deserializeString()
{
    std::string out;
    if (in.eof())
    {
        std::cerr << "Archive::deserializeString: EOF at beginning of string"
                  << std::endl;
    }
    else if (in.get() != '"')
    {
        std::cerr << "Archive::deserializeString: expected opening \""
                  << std::endl;
    }
    else
    {
        while (!in.eof())
        {
            char c = in.get();
            if (c == '"')
            {
                break;
            }
            else if (c == '\\')
            {
                if (!in.eof())
                {
                    out.push_back(in.get());
                }
            }
            else
            {
                out.push_back(c);
            }
        }
    }
    return out;
}


}   // namespace Kernel
