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

#include "libfive/tree/serializer.hpp"

namespace Kernel {

const uint8_t Serializer::END_OF_ITEM = 0xFF;

void Serializer::run(Archive& a)
{
    static_assert(Opcode::LAST_OP <= 254, "Too many opcodes");

    for (const auto& s : a.shapes)
    {
        serializeShape(s);
    }
}

void Serializer::serializeTree(Tree t)
{
    for (auto& n : t.ordered())
    {
        // Skip this id, as it has already been stored
        if (ids.find(n.id()) != ids.end())
        {
            continue;
        }
        if (n->op == Opcode::ORACLE)
        {
            assert(n->oracle.get() != nullptr);
            for (auto& d : n->oracle->dependencies())
            {
                serializeTree(d);
            }
        }
        out.put(n->op);
        ids.insert({ n.id(), (uint32_t)ids.size() });

        // Write constants as raw bytes
        if (n->op == Opcode::CONSTANT)
        {
            serializeBytes(n->value);
        }
        else if (n->op == Opcode::ORACLE)
        {
            assert(n->oracle.get() != nullptr);
            serializeString(n->oracle->name());
            OracleClause::serialize(n->oracle->name(), n->oracle.get(), *this);
        }

        switch (Opcode::args(n->op))
        {
            case 2:  serializeBytes(ids.at(n->rhs.get())); // FALLTHRU
            case 1:  serializeBytes(ids.at(n->lhs.get())); // FALLTHRU
            default: break;
        }
    }
}

void Serializer::serializeShape(const Archive::Shape& s)
{
    // 'T' indicate a fully-serialized tree;
    // 't' indicates an id pointing to an earlier tree.
    const bool already_stored = ids.find(s.tree.id()) != ids.end();
    out.put(already_stored ? 't' : 'T');

    serializeString(s.name);
    serializeString(s.doc);

    if (already_stored)
    {
        serializeBytes(ids.at(s.tree.id()));
    }
    else
    {
        serializeTree(s.tree);
        out.put(END_OF_ITEM);
    }
    for (auto& v : s.vars)
    {
        auto a = ids.find(v.first);
        if (a == ids.end())
        {
            std::cerr << "Archive::serialize: named variable not found.";
        }
        else
        {
            serializeString(v.second);
            serializeBytes(a->second);
        }
    }
    out.put(END_OF_ITEM);
}

void Serializer::serializeString(const std::string& s)
{
    out.put('"');
    for (auto& c : s)
    {
        if (c == '"' || c == '\\')
        {
            out.put('\\');
        }
        out.put(c);
    }
    out.put('"');
}


}   // namespace Kernel
