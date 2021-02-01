/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#pragma once

#include <Eigen/Eigen>
#include <boost/container/small_vector.hpp>

#include "libfive/tree/tree.hpp"

#include "libfive/eval/eval_array_size.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/eval/feature.hpp"
#include "libfive/eval/tape.hpp"

#include "libfive/oracle/oracle_context.hpp"

namespace libfive {

class BaseEvaluator;

/* The oracle is an interface class for the use of externally defined oracles
 * as primitives in trees.
 */

class Oracle
{
public:
    virtual ~Oracle()=default;

    /*
     *  Sets a particular position value, for use in any of the eval functions.
     *  This should be O(1) for best performance.
     *  The oracle must accept 0 <= index < LIBFIVE_EVAL_ARRAY_SIZE.
     */
    virtual void set(const Eigen::Vector3f& p, size_t index=0)=0;

    /*
     *  Sets a region for interval evaluation
     */
    virtual void set(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper)=0;

    /*
     *  Sets a variable.  Returns whether anything changed as a result.
     */
    virtual bool setVar(Tree::Id var, float value) { return false; }

    /*
     *  Return the result of interval arithmetic over the range previously
     *  defined with set(Interval).  If the output range could include
     *  NaN, the oracle must return the interval {NaN, NaN}.
     */
    virtual void evalInterval(Interval& out)=0;

    /*
     *  Re-implemented by subclasses to return a context that specializes
     *  the oracle for operations on the most recent interval region, which
     *  must have been assigned with set() and evaluated with evalInterval.
     */
    virtual std::shared_ptr<OracleContext> push(Tape::Type t)
    {
        (void)t;
        return std::shared_ptr<OracleContext>(nullptr);
    }

    /*
     *  Returns the result of pointwise arithemetic on the value
     *  previously defined with set(Eigen::Vector3f, index)
     */
    virtual void evalPoint(float& out, size_t index=0)=0;

    /*
     *  Block-level floating-point evaluation.
     *  By default, this simply calls evalPoint multiple times; overload it with
     *  a more efficient implementation if possible.
     */
    virtual void evalArray(
            Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                                      LIBFIVE_EVAL_ARRAY_SIZE,
                                      Eigen::RowMajor>,
                         1, Eigen::Dynamic> out)
    {
        for (unsigned i=0; i < out.cols(); ++i)
        {
            evalPoint(out(i), i);
        }
    }

    /*
     *  Sets appropriate bits to 1 if the given point (as set with
     *  set(Eigen::Vector3f, i) and evaluated with evalArray) is ambiguous.
     *
     *  This function should set bits to indicate ambiguity in the oracle,
     *  but should not clear bits (as they may have been set by other oracles
     *  or evaluators).
     *
     *  It must only be called after evalArray is called
     *  (with the same result block size).
     */
    virtual void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic> out)=0;

    /*
     *  Returns the result of gradient arithemetic on the value
     *  previously defined with set(Eigen::Vector3f).
     *
     *  Note that the argument is the output, not an input, for parallel
     *  structure with other functions within this class.
     *
     *  In the case of ambiguous points, only one gradient is returned.
     */
    virtual void evalDerivs(
            Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                         3, 1, true> out, size_t index=0)=0;

    /*
     *  Block-level floating-point evaluation.
     *
     *  By default, this simply calls evalDerivArray multiple times; overload it with
     *  a more efficient implementation if possible.
     *
     *  This function must only be called after evalArray is called
     *  (with the same output block size).
     */
    virtual void evalDerivArray(
            Eigen::Block<Eigen::Array<float, 3, LIBFIVE_EVAL_ARRAY_SIZE>,
                         3, Eigen::Dynamic, true> out)
    {
        Eigen::Array<float, 3, Eigen::Dynamic> dummy(3, out.cols());
        for (unsigned i=0; i < out.cols(); ++i)
        {
            evalDerivs(dummy.col(i), i);
        }
        out = dummy;
    }

    /*  Returns the set of features at the point stored in slot 0.  */
    virtual void evalFeatures(
            boost::container::small_vector<Feature, 4>& out)=0;

    /*  Returns the set of clauses that this is dependent on 
     *  given the specified context.*/
    virtual boost::container::small_vector<Clause::Id, 4> 
            activeDependencies(OracleContext* context) 
    {
      return {};
    }

    /*
     *  Oracles are evaluated within a particular context,
     *  which is bound by this function.  This is used in cases where
     *  the oracle evaluation can be simplified by knowing that it's
     *  being evaluated within a particular spatial region.
     * 
     *  Oracles are also evaluated by an evaluator, which is referenced by this
     *  function.  This is used in cases where the oracle may need to reference
     *  information already present in the evaluator in order to deduplicate 
     *  work.  If not null, the evaluator may be assumed to be in use, and of
     *  the correct type for the evaluation function currently being called, and
     *  to be built from a deck that was made from the map that was passed in to
     *  OracleClause::getOracle to generate this.  (If this was generated via
     *  the version that does not take a map, or via any method other than 
     *  getOracle(), the evaluator passed in here must be null.)  Conversely, if
     *  the evaluator is null, it may be assumed that the Oracle was not generated
     *  via the map-using overload of getOracle.
     */
    void bind(std::shared_ptr<OracleContext> context, const BaseEvaluator* eval)
    {
        this->context = context;
        this->evaluator = eval;
    }

    void unbind()
    {
        this->context = nullptr;
        this->evaluator = nullptr;
    }

protected:
    std::shared_ptr<OracleContext> context;
    const BaseEvaluator*           evaluator;
};

} //Namespace Kernel

