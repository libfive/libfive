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
#include "libfive/tree/transformed_oracle_clause.hpp"
#include "libfive/eval/transformed_oracle.hpp"
#include "libfive/tree/archive.hpp"

namespace Kernel {

REGISTER_ORACLE_CLAUSE(TransformedOracleClause);

TransformedOracleClause::TransformedOracleClause(
        Tree underlying, Tree X_, Tree Y_, Tree Z_)
    : underlying(underlying), X_(X_), Y_(Y_), Z_(Z_)
{
    assert(underlying->op == Opcode::ORACLE);
    assert(underlying->oracle.get() != nullptr);
}

std::unique_ptr<Oracle> TransformedOracleClause::getOracle() const
{
    return std::unique_ptr<TransformedOracle>(
        new TransformedOracle(underlying->oracle->getOracle(), X_, Y_, Z_));
}

std::unique_ptr<const OracleClause>
TransformedOracleClause::remap(Tree self, Tree X_, Tree Y_, Tree Z_) const
{
    (void)self; // unused, as we execute the remap on the underlying oracle

    return std::unique_ptr<const OracleClause>(
        new TransformedOracleClause(underlying,
            this->X_.remap(X_, Y_, Z_),
            this->Y_.remap(X_, Y_, Z_),
            this->Z_.remap(X_, Y_, Z_)));
}

std::vector<Kernel::Tree> TransformedOracleClause::dependencies() const
{
    return { underlying, X_, Y_, Z_ };
}

bool TransformedOracleClause::serialize(std::vector<uint8_t>& data,
     std::map<Tree::Id, uint32_t>& ids) const
{
    auto serializeId = [&data, &ids](Tree t)
    {
      assert(ids.find(t.id()) != ids.end());
      Archive::serializeBytes(ids[t.id()], data);
    };
    serializeId(underlying);
    serializeId(X_);
    serializeId(Y_);
    serializeId(Z_);
    return true;
}

std::unique_ptr<const OracleClause> TransformedOracleClause::deserialize(
    const uint8_t*& pos, const uint8_t* end,
    std::map<uint32_t, Tree>& ts)
{
    auto deserializeId = [&pos, &end, &ts]()
    {
        auto idx = Archive::deserializeBytes<uint32_t>(pos, end);
        auto location = ts.find(idx);
        assert(location != ts.end());
        return location->second;
    };
    auto underlying = deserializeId();
    auto X_ = deserializeId();
    auto Y_ = deserializeId();
    auto Z_ = deserializeId();
    return std::unique_ptr<TransformedOracleClause>(
            new TransformedOracleClause(underlying, X_, Y_, Z_));
}

}; //namespace Kernel
