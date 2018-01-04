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
#pragma once

#include <map>
#include "libfive/tree/tree.hpp"

namespace Kernel {

struct Template
{
    Template(Tree t) : tree(t) { /* Nothing to do here */ }

    /*
     *  Serialize to a set of raw bytes
     */
    std::vector<uint8_t> serialize() const;

    /*
     *  Deserialize from a set of raw bytes
     */
    static Template deserialize(const std::vector<uint8_t>& data);

    /*
     *  Serialize a string, wrapping in quotes and escaping with backslash
     */
    static void serializeString(const std::string& s, std::vector<uint8_t>& out);

    /*
     *  Serialize an arbitrary set of bytes
     */
    template <typename T>
    static void serializeBytes(T t, std::vector<uint8_t>& out)
    {
        for (unsigned i=0; i < sizeof(t); ++i)
        {
            out.push_back(((uint8_t*)&t)[i]);
        }
    }

    template <typename T>
    static T deserializeBytes(const uint8_t*& pos, const uint8_t* end)
    {
        T t;
        for (unsigned i=0; i < sizeof(t) && pos < end; ++i)
        {
            ((uint8_t*)&t)[i] = *pos++;
        }
        return t;
    }

    /*
     *  Deserializes a string, handling escaped characters
     *  Moves pos along, returning early if it hits end
     */
    static std::string deserializeString(const uint8_t*& pos,
                                         const uint8_t* end);

    /*  Root tree  */
    Tree tree;

    std::string name;
    std::string doc;

    std::map<Tree::Id, std::string> vars;
};

}   // namespace Kernel
