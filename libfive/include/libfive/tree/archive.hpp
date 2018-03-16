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

class Archive
{
public:
    Archive() { /* Nothing to do here */ }
    Archive(Tree t) { addShape(t); }

    /*
     *  Adds a new shape to the archive
     */
    void addShape(Tree tree, std::string name="", std::string doc="",
                  std::map<Tree::Id, std::string> vars=
                    std::map<Tree::Id, std::string>());

    /*
     *  Serialize to a set of raw bytes
     */
    std::vector<uint8_t> serialize() const;

    /*
     *  Deserialize from a set of raw bytes
     */
    static Archive deserialize(const std::vector<uint8_t>& data);

    /*  Stores a tree, plus metadata to describe it.
     *  This could be used to store shape functions, e.g.
     *  circle(x, y, r), where vars have their own names and docstrings
     */
    struct Shape {
        Shape() : tree(Tree::Invalid()) {}

        Tree tree;

        std::string name;
        std::string doc;

        std::map<Tree::Id, std::string> vars;
    };

    /* Here's the actual data stored in the Archive */
    std::list<Shape> shapes;

    /**************************************************************************
     *  HELPER FUNCTIONS
     *  These are exposed to make things like Oracle serialization easier.
     *************************************************************************/

    /*
     *  Serialize a string, wrapping in quotes and escaping with backslash
     *  This is exposed as a public function so Oracles can more easily
     *  serialize their names, etc.
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

    /*
     *  Deserializes a string, handling escaped characters
     *  Moves pos along, returning early if it hits end
     */
    static std::string deserializeString(const uint8_t*& pos,
                                         const uint8_t* end);

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

protected:
    /*
     *  Writes a tree to the given output stream.
     *
     *  Uses the ids map to only serialize trees that haven't already been
     *  stored.  This can be a problem if the root of the tree was already
     *  stored; in that case, it uses the lower-case 't' identifier to point
     *  back at the id.
     */
    void serializeShape(const Shape& s, std::vector<uint8_t>& out,
                        std::map<Tree::Id, uint32_t>& ids) const;

    /*
     *  Deserializes a Shape, moving pos along.
     */
    static Shape deserializeShape(const uint8_t*& pos, const uint8_t* end,
                                  std::map<uint32_t, Tree>& ts);

    /*  We use this flag to end a data stream */
    static const uint8_t END_OF_ITEM;
};

}   // namespace Kernel
