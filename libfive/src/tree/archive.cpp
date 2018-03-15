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
#include <iostream>
#include <cassert>

#include "libfive/tree/archive.hpp"

namespace Kernel
{

const uint8_t Archive::END_OF_ITEM = 0xFF;

void Archive::addShape(Tree tree, std::string name, std::string doc,
                       std::map<Tree::Id, std::string> vars)
{
    Shape s;
    s.tree = tree;
    s.name = name;
    s.doc = doc;
    s.vars = vars;

    shapes.push_back(s);
}

std::vector<uint8_t> Archive::serialize() const
{
    static_assert(Opcode::LAST_OP <= 254, "Too many opcodes");

    std::vector<uint8_t> out;
    std::map<Tree::Id, uint32_t> ids;

    for (const auto& s : shapes)
    {
        serializeShape(s, out, ids);
    }
    return out;
}

void Archive::serializeShape(const Shape& s, std::vector<uint8_t>& out,
                             std::map<Tree::Id, uint32_t>& ids) const
{
    // 'T' indicate a fully-serialized tree;
    // 't' indicates an id pointing to an earlier tree.
    const bool already_stored = ids.find(s.tree.id()) != ids.end();
    out.push_back(already_stored ? 't' : 'T');

    serializeString(s.name, out);
    serializeString(s.doc, out);

    if (already_stored)
    {
        serializeBytes(ids.at(s.tree.id()), out);
    }
    else
    {
        for (auto& n : s.tree.ordered())
        {
            out.push_back(n->op);
            ids.insert({n.id(), ids.size()});

            // Write constants as raw bytes
            if (n->op == Opcode::CONSTANT)
            {
                serializeBytes(n->value, out);
            }
            else if (n->op == Opcode::VAR_FREE)
            {
                auto a = s.vars.find(n.id());
                serializeString(a == s.vars.end() ? "" : a->second, out);
            }
            else if (n->op == Opcode::ORACLE)
            {
                assert(n->oracle.get() != nullptr);
                serializeString(n->oracle->name(), out);
                OracleClause::serialize(n->oracle->name(), n->oracle.get(),
                                        out);
            }
            switch (Opcode::args(n->op))
            {
                case 2:  serializeBytes(ids.at(n->rhs.get()), out); // FALLTHRU
                case 1:  serializeBytes(ids.at(n->lhs.get()), out); // FALLTHRU
                default: break;
            }
        }
        out.push_back(END_OF_ITEM);
    }
}

void Archive::serializeString(const std::string& s, std::vector<uint8_t>& out)
{
    out.push_back('"');
    for (auto& c : s)
    {
        if (c == '"' || c == '\\')
        {
            out.push_back('\\');
        }
        out.push_back(c);
    }
    out.push_back('"');
}

////////////////////////////////////////////////////////////////////////////////

Archive Archive::deserialize(const std::vector<uint8_t>& data)
{
    const uint8_t* pos = &*data.begin();
    const uint8_t* end = pos + data.size();

    std::map<uint32_t, Tree> ts;

    Archive out;

    while (pos != end)
    {
        out.shapes.push_back(deserializeShape(pos, end, ts));
    }
    return out;
}

Archive::Shape Archive::deserializeShape(const uint8_t*& pos, const uint8_t* end,
                                         std::map<uint32_t, Tree>& ts)
{
#define REQUIRE(cond) \
    if (!(cond)) \
    { \
        std::cerr << "Archive::deserialize: expected " << #cond \
                  << " at " << __LINE__ << std::endl; \
    }
#define CHECK_POS() REQUIRE(pos != end)

    CHECK_POS();
    const uint8_t tag = *pos++;
    REQUIRE(tag == 'T' || tag == 't');
    CHECK_POS();

    Shape out;
    out.name = deserializeString(pos, end);
    CHECK_POS();
    out.doc = deserializeString(pos, end);

    if (tag == 't')
    {
        auto root = deserializeBytes<uint32_t>(pos, end);
        out.tree = ts.at(root);
    }
    else
    {
        while (pos != end)
        {
            CHECK_POS();

            // Check for END_OF_ITEM as a demarcation between Shapes
            uint8_t op_ = *pos++;
            if (op_ == END_OF_ITEM)
            {
                break;
            }

            auto op = Opcode::Opcode(op_);

            REQUIRE(op > Opcode::INVALID);
            REQUIRE(op < Opcode::LAST_OP);
            CHECK_POS();

            auto args = Opcode::args(op);
            auto next = ts.size();
            if (op == Opcode::CONSTANT)
            {
                float v = deserializeBytes<float>(pos, end);
                ts.insert({next, Tree(v)});
            }
            else if (op == Opcode::VAR_FREE)
            {
                std::string var = deserializeString(pos, end);
                auto v = Tree(op);
                out.vars.insert({v.id(), var});
                ts.insert({next, v});
            }
            else if (op == Opcode::ORACLE)
            {
                std::string name = deserializeString(pos, end);
                CHECK_POS();
                auto o = OracleClause::deserialize(name, pos, end);
                if (o.get() == nullptr)
                {
                    std::cerr
                        << "Archive::deserialize: failed to deserialize Oracle \""
                        << name << "\"" << std::endl;
                    return out;
                }
                ts.insert({next, Tree(std::move(o))});
            }
            else if (args == 2)
            {
                auto rhs = deserializeBytes<uint32_t>(pos, end);
                auto lhs = deserializeBytes<uint32_t>(pos, end);
                ts.insert({next, Tree(op, ts.at(lhs), ts.at(rhs))});
            }
            else if (args == 1)
            {
                auto lhs = deserializeBytes<uint32_t>(pos, end);
                ts.insert({next, Tree(op, ts.at(lhs))});
            }
            else
            {
                ts.insert({next, Tree(op)});
            }
        }
        out.tree = ts.at(ts.size() - 1);
    }

    return out;
}

std::string Archive::deserializeString(const uint8_t*& pos, const uint8_t* end)
{
    std::string out;
    if (pos == end)
    {
        std::cerr << "Archive::deserializeString: EOF at beginning of string"
                  << std::endl;
    }
    else if (*pos++ != '"')
    {
        std::cerr << "Archive::deserializeString: expected opening \""
                  << std::endl;
    }
    else
    {
        while (pos != end)
        {
            auto c = *pos++;
            if (c == '"')
            {
                break;
            }
            else if (c == '\\')
            {
                if (pos != end)
                {
                    out.push_back(*pos++);
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
