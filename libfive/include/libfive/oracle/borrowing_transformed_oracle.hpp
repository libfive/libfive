/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/oracle/oracle.hpp"
#include "libfive/eval/clause.hpp"

namespace libfive {

/* The borrowingTransformedOracle is the result of generating an oracle
 * from a transformedOracleClause while using a deck for deduplication.
 */

class BorrowingTransformedOracle : public Oracle
{
public:
    BorrowingTransformedOracle(std::unique_ptr<Oracle> underlying, 
                               Clause::Id X_, Clause::Id Y_, Clause::Id Z_);

    /* The set methods are no-ops, since we're getting our input information
     * from the deck.*/
    void set(const Eigen::Vector3f&, const Eigen::Vector3f&) override {}
    void set(const Eigen::Vector3f&, size_t) override {}

    void evalInterval(Interval& out) override;

    void evalPoint(float& out, size_t index=0) override;

    void evalArray(
        Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                     LIBFIVE_EVAL_ARRAY_SIZE,Eigen::RowMajor>,
                     1, Eigen::Dynamic> out) override;

    void checkAmbiguous(
        Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                     1, Eigen::Dynamic> out) override;

    void evalDerivs(
        Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                     3, 1, true> out, size_t index=0) override;

    void evalDerivArray(
        Eigen::Block<Eigen::Array<float, 3, LIBFIVE_EVAL_ARRAY_SIZE>,
                     3, Eigen::Dynamic, true> out) override;

    void evalFeatures(
        boost::container::small_vector<Feature, 4>& out) override;

    /*
     *  Returns an instance of Context (defined below).
     */
    std::shared_ptr<OracleContext> push(Tape::Type t) override;

    boost::container::small_vector<Clause::Id, 4> activeDependencies(
      OracleContext* context) override { return {X, Y, Z}; }

private:
    std::array<Clause::Id, 3> getRemappedXYZ() const;

    class Context : public OracleContext {
    public:
      std::shared_ptr<OracleContext> u;
      Clause::Id remappedX;
      Clause::Id remappedY;
      Clause::Id remappedZ;
      /*  A borrowingTransformedOracle is always considered "terminal" if the
       *  context for the underlying oracle is terminal; while its X, Y, and Z
       *  may have non-terminal elements, they will be accessed separately, due
       *  to being inside the deck and being returned via 
       *  BorrowingTransformedOracle::activeDependencies.
       */
      bool isTerminal() override { return (!u || u->isTerminal()); }

      void applyRemaps(const std::vector<Clause::Id>& remaps);
    };

    const std::unique_ptr<Oracle> underlying;
    Clause::Id X;
    Clause::Id Y;
    Clause::Id Z;
};



} //Namespace Kernel

