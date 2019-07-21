/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/tree/tree.hpp"

namespace libfive {

/*
*  TransformedOracleClause is used for implementing remap on OracleClauses
*/
class TransformedOracleClause: public OracleClause
{
public:
    /*
     *  underlying must be an OracleClause, but is stored in a Tree to
     *  keep track of ownership, since that's handled at the Tree level.
     */
    TransformedOracleClause(Tree underlying, Tree X_, Tree Y_, Tree Z_);

    std::unique_ptr<Oracle> getOracle() const override;
    std::string name() const override { return "TransformedOracleClause"; }

    /*
     *  More efficient remap implementation that remaps the underlying Trees
     */
    std::unique_ptr<const OracleClause> remap(
            Tree self, Tree X_, Tree Y_, Tree Z_) const override;

    std::vector<libfive::Tree> dependencies() const override;

    bool serialize(Serializer& out) const;
    static std::unique_ptr<const OracleClause> deserialize(Deserializer& in);

private:
    Tree underlying;
    Tree X_;
    Tree Y_;
    Tree Z_;
};

} //namespace libfive
