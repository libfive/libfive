/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <functional>
#include <memory>
#include <map>
#include <string>
#include <iostream>

#include "libfive/tree/tree.hpp"

namespace libfive {

/*  Forward declaration */
class Oracle;
class Serializer;
class Deserializer;

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
    virtual std::vector<libfive::Tree> dependencies() const
    {
        return {};
    }

    /*
     *  Installs a particular class's serializer / deserializer pair
     *  T must be a subclass of OracleClause, and install must be called
     *  with the class's name().
     *
     *  In addition, T must declare
     *      bool T::serialize(Serializer& out) const
     *      static std::unique_ptr<const OracleClause> deserialize(
     *          Deserializer& in);
     *  If it depends on other trees having been serialized first, it should
     *      override dependencies().
     */
    template <class T>
    static void install(const std::string& name)
    {
        OracleSerializer ser = [](const OracleClause* c, Serializer& out)
        {
            auto o = dynamic_cast<const T*>(c);
            if (o == nullptr)
            {
                std::cerr << "OracleClause: Could not cast to specified type";
                return false;
            }
            return o->serialize(out);
        };
        OracleDeserializer de = T::deserialize;
        installed()[name] = { ser, de };
    }

    /*
     *  Serializes an oracle clause by looking up an installed serializer.
     *      data is pushed back into the data vector
     *      returns false on failure
     */
    static bool serialize(const std::string& name, const OracleClause*,
                          Serializer& ser);

    /*
     *  Deserializes an oracle clause by looking up an installed deserializer.
     *      pos is adjusted as the data is read.
     *      returns a null pointer on failure.
     */
    static std::unique_ptr<const OracleClause> deserialize(
            const std::string& name, Deserializer& in);

    /*
     *  Executes a coordinate transform on the underlying oracle.
     *  The default implementation uses TransformedOracleClause, but subclasses
     *  could specialize to take advantage of any unique structures.
     *
     *  self is a Tree that contains the OracleClause; it is passed in to
     *  handle cases where the underlying OracleClause needs to be cloned,
     *  as ownership is handled at the Tree level.
     */
    virtual std::unique_ptr<const OracleClause> remap(
            Tree self, Tree X_, Tree Y_, Tree Z_) const;

protected:
    typedef std::function<bool(const OracleClause*, Serializer&)>
        OracleSerializer;
    typedef std::function<std::unique_ptr<const OracleClause>(Deserializer&)>
        OracleDeserializer;

    typedef std::pair<OracleSerializer, OracleDeserializer> SerDe;

    /*
     *  We use this function to work around static initialization
     *  order dependencies.
     *  https://isocpp.org/wiki/faq/ctors#construct-on-first-use-v2
     */
    static std::map<std::string, SerDe>& installed()
    {
        static std::map<std::string, SerDe> m;
        return m;
    }
};

}   // namespace libfive

/*
 *  Use this macro in the .cpp file for classes derived from OracleClause
 *  to automatically register them for serialization / deserialization.
 */
#define REGISTER_ORACLE_CLAUSE(T) \
class T##_Installer { \
public: \
    T##_Installer() { \
        OracleClause::install<T>(#T); \
    }\
};\
static T##_Installer T##_Installer_Instance;
