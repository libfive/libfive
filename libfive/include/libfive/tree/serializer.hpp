/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <iostream>
#include <map>

#include "libfive/tree/archive.hpp"

namespace libfive {

class Serializer
{
public:
    Serializer(std::ostream& out)
        : out(out)
    { /* Nothing to do here */ }

    /*
     *  Main serialization function
     */
    void run(Archive& a);

    /*
     *  Serialize a string, wrapping in quotes and escaping with backslash
     *  This is exposed as a public function so Oracles can more easily
     *  serialize their names, etc.
     */
    void serializeString(const std::string& s);

    /*
     *  Serialize / deserialize an arbitrary object as raw bytes
     */
    template <typename T> void serializeBytes(T t)
    {
        out.write(reinterpret_cast<char*>(&t), sizeof(t));
    }

    /*  This variable maps between Tree ids and positions within
     *  the serialized data stream, so we can skip trees that
     *  have already been serialized.  */
    std::map<Tree::Id, uint32_t> ids;

    /*  We use this flag to end a data stream, either of
     *  the tree of a shape or its vars.  */
    static const uint8_t END_OF_ITEM;

protected:
    /*
     *  Serialize a Tree and all of its dependencies.
     *  Modifies the ids map to store where each subtree has been serialized.
     */
    void serializeTree(Tree t);

    /*
     *  Writes a tree to the given output stream.
     *
     *  Uses the ids map to only serialize trees that haven't already been
     *  stored.  This can be a problem if the root of the tree was already
     *  stored; in that case, it uses the lower-case 't' identifier to point
     *  back at the id.
     */
    void serializeShape(const Archive::Shape& s);

    std::ostream& out;
};

}   // namespace libfive
