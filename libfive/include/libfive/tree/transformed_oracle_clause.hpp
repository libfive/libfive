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

/*  Forward declaration */
class TransformedOracle;

/*
*  TransformedOracleClause is the result of remapping an OracleClause.
*/
class TransformedOracleClause: public OracleClause
{
public:
    std::unique_ptr<Oracle> getOracle() const override;
    /* TransformedOracle may require deduplication if the same transformation
     * is applied to the same oracle on two separate occasions, so this is 
     * handled via a static member of the class.  This also allows multiple
     * transformations to be combined into one if no other operations are
     * in between.
     */
    static std::shared_ptr<const TransformedOracleClause> transform(
        const std::shared_ptr<const OracleClause> underlying,
        Tree X_, Tree Y_, Tree Z_);

    std::string name() const override { return "TransformedOracleClause"; }

private:
    TransformedOracleClause(const std::shared_ptr<const OracleClause> underlying,
        Tree X_, Tree Y_, Tree Z_);

    //For deduplication purposes:
    typedef std::pair<std::array<Tree::Id, 3>, const OracleClause*> Key;
    static std::map<Key, std::weak_ptr<TransformedOracleClause>> 
        transformedOracles;

    const std::shared_ptr<const OracleClause> underlying;
    Tree X_;
    Tree Y_;
    Tree Z_;
};

}; //namespace Kernel
