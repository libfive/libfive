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

#include "libfive/render/ipow.hpp"
#include "libfive/render/simplex/simplex.hpp"

#include "libfive/eval/eval_deriv_array.hpp"

namespace Kernel {

template <unsigned N>
class SimplexTree
{
public:
    /*  Per-simplex vertex positions.
     *  These are indexed using the ternary scheme described in neighbors.hpp */
    Eigen::Matrix<double, N + 1, ipow(3, N)> vertices;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<const SimplexTree<N>>, 1 << N> children;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

protected:
    /*
     *  Private constructor for SimplexTree
     *
     *  This constructor recurses until the minimum dimension of region
     *  is less than min_feature, subdividing regions evenly into 2^N
     *  subregions at each step.  Then, regions are subdivided until
     *  the summed QEF error is less than max_error.
     *
     *  If multiple evaluators are provided, then tree construction will
     *  be distributed across multiple threads.
     */
    SimplexTree(DerivArrayEvaluator* eval, Simplex<N, (1 << N) - 1> region,
                double min_feature, double max_err);
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel
