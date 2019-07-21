/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#pragma once

#include <Eigen/Eigen>

#include "libfive/oracle/oracle.hpp"
#include "libfive/eval/eval_array_size.hpp"

namespace libfive {

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

}   // namespace libfive
