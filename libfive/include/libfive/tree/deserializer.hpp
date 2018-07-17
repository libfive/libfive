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

#include "libfive/tree/archive.hpp"

namespace Kernel {

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

}   // namespace Kernel
