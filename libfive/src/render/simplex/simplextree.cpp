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

#include <numeric>

#include "libfive/render/simplex/simplextree.hpp"
#include "libfive/render/simplex/ternary.hpp"

namespace Kernel {

template <unsigned N>
SimplexTree<N>::SimplexTree(
        DerivArrayEvaluator* eval,
        Simplex<N, (1 << N) - 1> region,
        double min_feature, double max_err)
{
    if (((region.upper - region.lower) > min_feature).any())
    {
        recurse(eval, region, min_feature, max_err);
        return;
    }

    // Store values for the cell corner evaluation
    for (unsigned i=0; i < children.size(); ++i)
    {
        Eigen::Vector3f p;
        p << region.corner(i).template cast<float>(),
             Eigen::Array<float, 3 - N, 1>::Zero();
        eval->set(p, i);
    }

    // Find the values + derivatives
    auto ds = eval->derivs(children.size());

    // Total QEF error (used to decide whether to recurse
    std::array<double, ipow(3, N)> errors;
    std::array<bool, ipow(3, N)> done;
    std::fill(done.begin(), done.end(), false);

    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        const auto t = ternary<N>(i);

        // Find the number of corners in this simplex
        const unsigned rows = std::accumulate(t.begin(), t.end(), 1,
                [](unsigned prod, int i) { return prod * (i ? 1 : 2); });

        //  The A matrix is of the form
        //  [n1x, n1y, n1z, -1]
        //  [n2x, n2y, n2z, -1]
        //  [n3x, n3y, n3z, -1]
        //  ...
        //  (with one row for each sampled point's normal)
        Eigen::Matrix<double, Eigen::Dynamic, N + 1> A(rows, N + 1);

        //  The b matrix is of the form
        //  [(p1 - center, w1) . (n1, -1)]
        //  [(p2 - center, w2) . (n2, -1)]
        //  [(p3 - center, w3) . (n3, -1)]
        //  ...
        //  (with one row for each sampled point)
        Eigen::Matrix<double, Eigen::Dynamic, 1> b(rows, 1);

        // Write the appropriate corner values to the A and B matrices.
        // We loop over every corner, filter which ones are in this particular
        // simplex, and only store their values.
        unsigned r=0;
        for (unsigned j=0; j < children.size(); ++j)
        {
            // Check if this corner is in the simplex
            bool included = true;
            for (unsigned a=0; a < t.size(); ++a)
            {
                switch (t[a])
                {
                    case  0:    break;
                    case -1:    included &= !(j & (1 << a)); break;
                    case  1:    included &=  (j & (1 << a)); break;
                    default:    assert(false); break;
                }
            }

            if (included)
            {
                A.row(r) << ds.col(j).template head<N>()
                                     .template cast<double>()
                                     .transpose(),
                            -1;

                Eigen::Matrix<double, N + 1, 1> p;
                p << region.corner(j), ds(3, j);
                b(r) = A.row(r) * p;

                r++;
            }
        }
        assert(r == rows);
    }

    if (std::accumulate(errors.begin(), errors.end(), 0.0) > max_err)
    {
        recurse(eval, region, min_feature, max_err);
    }
}

template <unsigned N>
void SimplexTree<N>::recurse(
        DerivArrayEvaluator* eval, Simplex<N, (1 << N) - 1> region,
        double min_feature, double max_err)
{
    auto rs = region.subdivide();
    for (unsigned i=0; i < children.size(); ++i)
    {
        children[i].reset(
                new SimplexTree<N>(eval, rs[i], min_feature, max_err));
    }
}

}   // namespace Kernel
