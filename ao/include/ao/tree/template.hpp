#pragma once

#include <map>
#include "ao/tree/tree.hpp"

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
