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
    /*
     *  Constructs the SimplexTree object, populating type and pushing into
     *  the given evaluator based in interval arithmetic results.
     *
     *  It is the caller's responsability to pop() the evaluator.
     */
    SimplexTree(XTreeEvaluator* eval, Region<N> region, unsigned depth=0);

    /*  Per-simplex vertex positions and inside / outside state.
     *  These are indexed using the ternary scheme described in neighbors.hpp */
    Eigen::Matrix<double, N + 1, ipow(3, N)> vertices;
    std::array<bool, ipow(3, N)> inside;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<SimplexTree<N>>, 1 << N> children;

    /*  Empty / filled / ambiguous */
    Interval::State type = Interval::UNKNOWN;

    /*  Marked as true when the QEF error is small enough */
    bool complete = false;

    /*  Tree depth (0 = root) */
    const unsigned depth;

    /*  Evaluation region */
    const Region<N> region;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0].get() != nullptr; }

    /*
     *  Looks up a child, returning this if this isn't a branch
     */
    SimplexTree<N>* child(unsigned i)
    { return isBranch() ? children[i].get() : this; }


    /*
     *  Populates vertices with per-simplex vertex positioning.
     *  Returns the total error.
     */
    double findVertices(XTreeEvaluator* eval);

    /*
     *  Populates vertices.col(N) with the true vertex position, and
     *  populates inside[] with correct results.
     */
    void checkVertices(XTreeEvaluator* eval);
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel
