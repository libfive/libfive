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

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/eval/eval_complete.hpp"

namespace Kernel {

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

    void evalInterval(Interval::I& out) override;
    void evalPoint(float& out, size_t index) override;
    void evalArray(
        Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                     LIBFIVE_EVAL_ARRAY_SIZE,Eigen::RowMajor>,
                     1, Eigen::Dynamic> out) override;

    void checkAmbiguous(
        Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                     1, Eigen::Dynamic> out) override;

    void evalDerivs(
        Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                     3, 1, true> out, size_t index) override;

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

    void setUnderlyingArrayValues(int count);

    const std::unique_ptr<Oracle> underlying;
    CompleteEvaluator xEvaluator;
    CompleteEvaluator yEvaluator;
    CompleteEvaluator zEvaluator;
};



} //Namespace Kernel

