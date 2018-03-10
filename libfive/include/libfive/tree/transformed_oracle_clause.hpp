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

#include "libfive/tree/oracle_clause.hpp"
#include "libfive/tree/tree.hpp"

namespace Kernel {

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

private:
    Tree underlying;
    Tree X_;
    Tree Y_;
    Tree Z_;
};

}; //namespace Kernel
