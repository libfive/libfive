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

#include <Eigen/Eigen>
#include <boost/container/small_vector.hpp>

#include "libfive/eval/eval_array_size.hpp"
#include "libfive/eval/interval.hpp"
#include "libfive/eval/feature.hpp"

namespace Kernel {

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
     *  Return the result of interval arithmetic over the range
     *  previously defined with set(Interval::I)
     */
    virtual void evalInterval(Interval::I& out)=0;

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
     *  set(Eigen::Vector3f, i) and evaluated with evaluArray) is ambiguous.
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
     *  By default, this simply calls evalDerivArray multiple times; overload it with
     *  a more efficient implementation if possible.
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

    /*
    *  Evaluates an interval as above, but also performs the equivalent
    *  (if any) of a tape push.
    */
    virtual void evalAndPushInterval(Interval::I& out) {
      evalInterval(out);
    }

    /*
    *  Evaluates an point as above, but also performs the equivalent
    *  (if any) of a tape push.
    */
    virtual void evalAndPushPoint(float& out, size_t index = 0) {
      evalPoint(out, index);
    }

    /*
    *  Evaluates an point, regardless of whether it is contained in the
    *  region the oracle has been pushed into.
    */
    virtual void baseEvalPoint(float& out, size_t index = 0) {
      evalPoint(out, index);
    }

    /*
    *  Evaluates the gradient, regardless of whether the point is contained 
    *  in the region the oracle has been pushed into.
    */
    virtual void baseEvalDerivs(
      Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
      3, 1, true> out, size_t index = 0) {
      evalDerivs(out, index);
    }

    /*  Performs the equivalent (if any) of a tape pop.  */
    virtual void pop() {}
};

} //Namespace Kernel

