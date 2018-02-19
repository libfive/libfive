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

namespace Kernel {

/* The oracle is an interface class for the use of externally defined oracles
 * as primitives in trees.
 */

class Oracle
{
public:
    virtual ~Oracle()=default;

    /*
     *  Return the result of interval arithmetic over the range
     *  previously defined with set(Interval::I)
     */
    virtual void evalInterval(Interval::I& i)=0;

    /*
     *  Returns the result of pointwise arithemetic on the value
     *  previously defined with set(Eigen::Vector3f)
     */
    virtual void evalPoint(float& i)=0;

    /*
     *  Block-level floating-point evaluation.
     *  By default, this simply calls evalPoint multiple times; overload it with
     *  a more efficient implementation if possible.
     */
    virtual void evalArray(
            Eigen::Block<Eigen::Array<float, Eigen::Dynamic, LIBFIVE_EVAL_ARRAY_SIZE, Eigen::RowMajor>,
                         1, Eigen::Dynamic> out)
    {
        for (unsigned i=0; i < out.cols(); ++i)
        {
            evalPoint(out(i));
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
     *  Check if the index-0 result is ambiguous
     */
    virtual bool isAmbiguous() {
        Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE> out;
        checkAmbiguous(out.head(1));
        return out(0);
    }

    /*
     *  Returns the result of gradient arithemetic on the value
     *  previously defined with set(Eigen::Vector3f).
     *
     *  Note that v is the output, not an input, for parallel structure
     *  with other functions within this class.
     *
     *  In the case of ambiguous points, only one gradient is returned.
     */
    virtual void evalDerivs(
            Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                         3, 1, true> v)=0;

    /*
     *  Sets a particular position value, for use in evalArray or evalDerivs
     *  This should be O(1) for best performance.
     */
    virtual void set(const Eigen::Vector3f& p, size_t index=0)=0;

    /*  Since the gradient may be discontinuous at the queried point, a vector
     *  is returned; in each entry of the vector, the first element of the pair
     *  represents the gradient in some area adjoining the passed point.  The
     *  second element of the pair represents a normalized epsilon indicating
     *  the domain of this gradient: for small scalar e and normalized vector
     *  n, the gradient at point + en is the one whose associated epsilon is
     *  closest to n.
     */
    virtual boost::container::small_vector<std::pair<Eigen::Vector3f, Eigen::Vector3f>, 1>
        getGradients(Eigen::Vector3f point) const = 0;

};

} //Namespace Kernel
