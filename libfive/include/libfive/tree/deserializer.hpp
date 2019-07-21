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

class Deserializer
{
public:
    Deserializer(std::istream& in)
        : in(in)
    { /* Nothing to do here */ }

    /*
     *  Main deserialization function
     */
    Archive run();

    /*  Helper functions */
    std::string deserializeString();
    template <typename T> T deserializeBytes()
    {
        T t;
        in.read(reinterpret_cast<char*>(&t), sizeof(t));
        return t;
    }

    /*  This is the parallel map to Serializer::ids */
    std::map<uint32_t, Tree> trees;

protected:
    /*
     *  Deserializes a Shape
     */
    Archive::Shape deserializeShape(char tag);

    std::istream& in;
    Archive archive;
};

}   // namespace libfive
