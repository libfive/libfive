/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/eval/evaluator.hpp"

namespace libfive {

/* The transformedOracle is the result of applying a remap to an oracle.
*/

class TransformedOracle : public OracleStorage<>
{
public:
    TransformedOracle(
        std::unique_ptr<Oracle> underlying, Tree X_, Tree Y_, Tree Z_);

    /*
     *  Sets not only OracleStorage's points,
     *  but also the array evaluators.
     */
    void set(const Eigen::Vector3f& p, size_t index=0) override;

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
     *  Returns a context that pushes into each evaluator and the underlying
     *  oracle.  The returned object is an instance of Context (defined below).
     */
    std::shared_ptr<OracleContext> push(Tape::Type t) override;

private:
    class Context : public OracleContext {
    public:
        Tape::Handle tx;
        Tape::Handle ty;
        Tape::Handle tz;
        std::shared_ptr<OracleContext> u;

        bool isTerminal() override;
    };

    const std::unique_ptr<Oracle> underlying;
    Evaluator xEvaluator;
    Evaluator yEvaluator;
    Evaluator zEvaluator;
};



} //Namespace Kernel

