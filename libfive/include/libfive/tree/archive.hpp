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
     *
     *  The shape has an optional name and docstring, plus a map of
     *  variables to names.
     */
    void addShape(Tree tree, std::string name="", std::string doc="",
                  std::map<Tree::Id, std::string> vars=
                    std::map<Tree::Id, std::string>());

    /*
     *  Serialize to a set of raw bytes
     */
    void serialize(std::ostream& out);

    /*
     *  Deserialize from a set of raw bytes
     */
    static Archive deserialize(std::istream& in);

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
};

}   // namespace Kernel
