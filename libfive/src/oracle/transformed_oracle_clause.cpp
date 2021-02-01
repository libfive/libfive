/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/oracle/transformed_oracle_clause.hpp"
#include "libfive/oracle/transformed_oracle.hpp"
#include "libfive/oracle/borrowing_transformed_oracle.hpp"

#include "libfive/tree/serializer.hpp"
#include "libfive/tree/deserializer.hpp"

namespace libfive {

REGISTER_ORACLE_CLAUSE(TransformedOracleClause)

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

std::unique_ptr<Oracle> TransformedOracleClause::getOracle(
    std::unordered_map<Tree::Id, Clause::Id> map) const
{
  auto X = map.find(X_.id());
  auto Y = map.find(Y_.id());
  auto Z = map.find(Z_.id());
  if (X == map.end() || Y == map.end() || Z == map.end()) {
    assert(false);
    return getOracle();
  }
  return std::make_unique<BorrowingTransformedOracle>(
        underlying->oracle->getOracle(), X->second, Y->second, Z->second);
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

std::unique_ptr<const OracleClause> TransformedOracleClause::remap(
    Tree                     self,
    std::map<Tree::Id, Tree> deps_) const
{
    auto lx = deps_.find(this->X_.id());
    auto ly = deps_.find(this->Y_.id());
    auto lz = deps_.find(this->Z_.id());

    auto Xn = lx == deps_.end() ? this->X_.remap(deps_) : lx->second;
    auto Yn = ly == deps_.end() ? this->Y_.remap(deps_) : ly->second;
    auto Zn = lz == deps_.end() ? this->Z_.remap(deps_) : lz->second;

    for (auto iter : { lx, ly, lz }) {
        if (iter != deps_.end()) {
            deps_.erase(iter);
        }
    }

    auto newUnderlying = deps_.empty() ? this->underlying 
                                       : this->underlying.remap(deps_);

    return std::unique_ptr<const OracleClause>(new TransformedOracleClause(
        newUnderlying, Xn, Yn, Zn));
}

std::vector<libfive::Tree> 
TransformedOracleClause::evaluationDependencies() const
{
    return { X_, Y_, Z_ };
}

std::vector<libfive::Tree> TransformedOracleClause::dependencies() const
{
    return { underlying, X_, Y_, Z_ };
}

bool TransformedOracleClause::serialize(Serializer& out) const
{
    auto serializeId = [&out](Tree t)
    {
      assert(out.ids.find(t.id()) != out.ids.end());
      out.serializeBytes(out.ids[t.id()]);
    };
    serializeId(underlying);
    serializeId(X_);
    serializeId(Y_);
    serializeId(Z_);
    return true;
}

std::unique_ptr<const OracleClause> TransformedOracleClause::deserialize(
        Deserializer& in)
{
    auto deserializeId = [&in]()
    {
        auto idx = in.deserializeBytes<uint32_t>();
        auto location = in.trees.find(idx);
        assert(location != in.trees.end());
        return location->second;
    };
    auto underlying = deserializeId();
    auto X_ = deserializeId();
    auto Y_ = deserializeId();
    auto Z_ = deserializeId();
    return std::unique_ptr<TransformedOracleClause>(
            new TransformedOracleClause(underlying, X_, Y_, Z_));
}

} //namespace libfive
