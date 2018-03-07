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

#include <functional>
#include <memory>
#include <map>
#include <string>
#include <iostream>

namespace Kernel {

/*  Forward declaration */
class Oracle;

/*
 *  OracleClause is an interface class for oracles, i.e. black-box nodes
 *  within a Tree.  The only API that it exposes is a way to get an Oracle;
 *  in the same way that Trees are turned into one or more Tapes for
 *  evaluation, OracleClauses are turned into one or more Oracles.
 */
class OracleClause
{
public:
    virtual ~OracleClause()=default;
    virtual std::unique_ptr<Oracle> getOracle() const=0;
    virtual std::string name() const=0;

    /*
     *  Installs a particular class's serializer / deserializer pair
     *  T must be a subclass of OracleClause, and install must be called
     *  with the class's name().
     *
     *  In addition, T must declare
     *      bool T::serialize(std::vector<uint8_t>& data) const
     *      static std::unique_ptr<const OracleClause> deserialize(
     *          const uint8_t*& pos, const uint8_t* end);
     */
    template <class T>
    static void install(const std::string& name)
    {
        Serializer ser = [](const OracleClause* c, std::vector<uint8_t>& data)
        {
            auto o = dynamic_cast<const T*>(c);
            if (o == nullptr)
            {
                std::cerr << "OracleClause: Could not cast to specified type";
                return false;
            }
            return o->serialize(data);
        };
        Deserializer de = T::deserialize;
        installed[name] = { ser, de };
    }

    /*
     *  Serializes an oracle clause by looking up an installed serializer.
     *      data is pushed back into the data vector
     *      returns false on failure
     */
    static bool serialize(const std::string& name,
            const OracleClause*, std::vector<uint8_t>& data);

    /*
     *  Deserializes an oracle clause by looking up an installed deserializer.
     *      pos is adjusted as the data is read.
     *      returns a null pointer on failure.
     */
    static std::unique_ptr<const OracleClause> deserialize
        (const std::string& name, const uint8_t*& pos, const uint8_t* end);

protected:
    typedef std::function<bool(const OracleClause*, std::vector<uint8_t>&)>
        Serializer;
    typedef std::function<std::unique_ptr<const OracleClause>
                          (const uint8_t*& pos, const uint8_t* end)>
        Deserializer;

    static std::map<std::string, std::pair<Serializer, Deserializer>> installed;
};

};
