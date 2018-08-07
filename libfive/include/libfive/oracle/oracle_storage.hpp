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

#include "libfive/oracle/oracle.hpp"
#include "libfive/eval/eval_array_size.hpp"

namespace Kernel {

template <int N=LIBFIVE_EVAL_ARRAY_SIZE>
class OracleStorage : public Oracle
{
public:
    void set(const Eigen::Vector3f& p, size_t index=0) override
    {
        points.col(index) = p;
    }

    void set(const Eigen::Vector3f& _lower,
             const Eigen::Vector3f& _upper) override
    {
        lower = _lower;
        upper = _upper;
    }


    /*
     *  Inefficient-but-correct implementation of evalDerivs
     *  (delegates work to evalFeatures)
     */
    void evalDerivs(
            Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                         3, 1, true> out, size_t index=0) override
    {
        Eigen::Vector3f before = points.col(0);
        points.col(0) = points.col(index);

        boost::container::small_vector<Feature, 4> fs;
        evalFeatures(fs);
        assert(fs.size() > 0);
        out = fs[0].deriv;

        points.col(0) = before;
    }

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

protected:
    /* Local storage for set(Vector3f) */
    Eigen::Array<float, 3, N> points;

    /* Local storage for set(Interval) */
    Eigen::Vector3f lower;
    Eigen::Vector3f upper;
};

}   // namespace Kernel
