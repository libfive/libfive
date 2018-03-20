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

#include "libfive/eval/oracle_storage.hpp"
#include "libfive/eval/bezier.hpp"

namespace Kernel {

/*
 *  Given a Bezier curve parameterized by a, b, c,
 *  returns a value t in the range [0, 1] that represents
 *  the nearest point along the curve.
 */
class BezierClosestPointOracle : public OracleStorage<>
{
public:
    /*
     *  Constructs a Bezier curve from a (at t=0) to c (at t=1).
     *  n is the number of segments to use in the linear approximation.
     */
    BezierClosestPointOracle(const Bezier& bezier, unsigned n=32);

    void evalInterval(Interval::I& out) override;
    void evalPoint(float& out, size_t index=0) override;

    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic> /* out */) override
    {
        // Nothing to do here
        // (not strictly correct, but close enough)
    }

    void evalDerivs(
            Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                         3, 1, true> out, size_t index=0) override;

    void evalFeatures(
            boost::container::small_vector<Feature, 4>& out) override;

protected:
    const Bezier bezier;

    /*
     *  [lower(i), upper(i)] is a line segment in the piecewise approximation
     *  of the underlying Bezier curve.
     */
    Eigen::Matrix<float, Eigen::Dynamic, 3> lower;
    Eigen::Matrix<float, Eigen::Dynamic, 3> upper;

    /*
     *  [lower_t(i), upper_t(i)] is the position along the curve (where t
     *  varies from 0 to 1)
     */
    Eigen::Matrix<float, Eigen::Dynamic, 1> lower_t;
    Eigen::Matrix<float, Eigen::Dynamic, 1> upper_t;
};

}   // namespace Kernel
