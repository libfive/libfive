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

#include "libfive/tree/transformed_oracle_clause.hpp" 
#include "libfive/eval/transformed_oracle.hpp"

namespace Kernel {

std::unique_ptr<Oracle> TransformedOracleClause::getOracle() const
{
    return std::make_unique<TransformedOracle>(
        underlying->getOracle(), X_, Y_, Z_);
}

std::shared_ptr<const TransformedOracleClause> 
TransformedOracleClause::transform(
    const std::shared_ptr<const OracleClause> underlying,
    Tree X_, Tree Y_, Tree Z_)
{
    auto underlyingAsTransformed =
        dynamic_cast<const TransformedOracleClause*>(underlying.get());
    if (underlyingAsTransformed != nullptr) //We can simplify.
    {
        return transform(underlyingAsTransformed->underlying,
            X_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_),
            Y_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_),
            Z_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_));
    }
    else {
        Key k{ { X_.id(), Y_.id(), Z_.id() }, underlying.get() };
        auto found = transformedOracles.find(k);
        if (found == transformedOracles.end())
        {
            auto transformed = 
                new TransformedOracleClause(underlying, X_, Y_, Z_);
            auto out = std::shared_ptr<TransformedOracleClause>(transformed);
            transformedOracles.insert({ k, out });
            return out;
        }
        else
        {
            assert(!found->second.expired());
            return found->second.lock();
        }
    }
}

TransformedOracleClause::TransformedOracleClause(
    const std::shared_ptr<const OracleClause> underlying,
    Tree X_, Tree Y_, Tree Z_) :
    underlying(underlying), X_(X_), Y_(Y_), Z_(Z_)
{
    //nothing to do here
}

std::map<TransformedOracleClause::Key, std::weak_ptr<TransformedOracleClause>> 
    TransformedOracleClause::transformedOracles;

}; //namespace Kernel