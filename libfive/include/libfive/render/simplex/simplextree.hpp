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
#include "libfive/render/brep/region.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

// Forward declaration
class XTreeEvaluator;
template <unsigned N> struct Simplex;

template <unsigned N>
class SimplexTree
{
public:
    /*  Per-simplex vertex positions and inside / outside state.
     *  These are indexed using the ternary scheme described in neighbors.hpp */
    Eigen::Matrix<double, N + 1, ipow(3, N)> vertices;
    std::array<bool, ipow(3, N)> inside;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<const SimplexTree<N>>, 1 << N> children;

    /*  Empty / filled / ambiguous */
    Interval::State type = Interval::UNKNOWN;

    /*  Depth within the tree structure (0 is the root) */
    const unsigned depth=0;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Private constructor for SimplexTree
     *
     *  This constructor recurses until the minimum dimension of region
     *  is less than min_feature, subdividing regions evenly into 2^N
     *  subregions at each step.  Then, regions are subdivided until
     *  the summed QEF error is less than max_error, or the depth is
     *  greater than max_depth (which only starts counting after we
     *  finish recursing due to min_feature).
     *
     *  If multiple evaluators are provided, then tree construction will
     *  be distributed across multiple threads.
     */
    SimplexTree(XTreeEvaluator* eval, Region<N> region,
                double max_feature, double min_feature, double max_err);

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0].get() != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const SimplexTree<N>* child(unsigned i) const
    { return isBranch() ? children[i].get() : this; }

protected:
    /*
     *  Private constructor that stores depth
     */
    SimplexTree(XTreeEvaluator* eval, Region<N> region, unsigned depth,
                double max_feature, double min_feature, double max_err);

    /*
     *  Populates the children array
     */
    void recurse(XTreeEvaluator* eval, Region<N> region,
                 double max_feature, double min_feature, double max_err);

    std::pair<bool, Eigen::Matrix<double, N + 1, 1>> unpack(
        const Simplex<N>& t, const Eigen::VectorXd& result,
        const Region<N>& region);
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel
