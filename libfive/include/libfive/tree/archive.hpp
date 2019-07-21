/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <map>

#include "libfive/tree/tree.hpp"

namespace libfive {

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

}   // namespace libfive
