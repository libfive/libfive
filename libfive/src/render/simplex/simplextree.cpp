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
#include "libfive/render/simplex/qef.hpp"

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

    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        const auto t = ternary<N>(i);

        // Find the number of corners in this simplex
        const unsigned rows = std::accumulate(t.begin(), t.end(), 1,
                [](unsigned prod, int i) { return prod * (i ? 1 : 2); });
        const unsigned cols = std::accumulate(t.begin(), t.end(), 0,
                [](unsigned sum, int i) { return sum + (i ? 0 : 1); });

        //  The A matrix is of the form
        //  [n1x, n1y, n1z, -1]
        //  [n2x, n2y, n2z, -1]
        //  [n3x, n3y, n3z, -1]
        //  ...
        //  (with one row for each sampled point's normal)
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> A(rows, cols + 1);

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
            if (isInSimplex(j, t))
            {
                Eigen::Matrix<double, Eigen::Dynamic, 1> p(cols + 1, 1);
                unsigned c = 0;
                for (unsigned a=0; a < N; ++a)
                {
                    if (t[a] == 0)
                    {
                        A(r, c) = ds(a, j);
                        p(c) = region.corner(j)(a);

                        c++;
                    }
                }
                assert(c == cols);
                A(r, c) = -1;

                p(c) = ds(3, j);
                b(r) = A.row(r) * p;

                r++;
            }
        }
        assert(r == rows);

        // Solve QEF here
        const Eigen::Matrix<double, Eigen::Dynamic, 1> result =
            A.colPivHouseholderQr().solve(b);

        // Store the error
        errors[i] = (A * result - b).squaredNorm();

        // Unpack the QEF solution into the vertex array
        unsigned c = 0;
        bool bounded = true;
        for (unsigned a=0; a < N; ++a)
        {
            if (t[a] == 0)
            {
                vertices(a, i) = result(c++);
                bounded &= (vertices(a, i) > region.lower(a));
                bounded &= (vertices(a, i) < region.upper(a));
            }
            else if (t[a] == -1)
            {
                vertices(a, i) = region.lower(a);
            }
            else if (t[a] == 1)
            {
                vertices(a, i) = region.upper(a);
            }
        }
        // Leave vertices(r, N) set to zero, because we'll refine it below

        // If the result is outside the boundary, loop from 0 to i,
        // picking out sub-simplices and using the one with minimum error
        if (!bounded)
        {
            double best_error = std::numeric_limits<double>::infinity();
            for (unsigned j=0; j < i; ++j)
            {
                const auto t_ = ternary<N>(j);
                bool is_sub_simplex = true;
                for (unsigned a=0; a < N; ++a)
                {
                    is_sub_simplex &= !t[a] || (t[a] == t_[a]);
                }
                if (is_sub_simplex && errors[j] < best_error)
                {
                    vertices.col(i) = vertices.col(j);
                    best_error = errors[j];
                }
            }
        }
    }

    // If the errors are too large, then recurse here
    if (std::accumulate(errors.begin(), errors.end(), 0.0) > max_err)
    {
        recurse(eval, region, min_feature, max_err);
        return;
    }

    // Otherwise, do a second evaluation pass to refine the values of F(p)
    // at the simplex corners, turning them from estimates to true evaluations.
    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        Eigen::Vector3f p;
        p << vertices.col(i).template head<N>().template cast<float>(),
             Eigen::Array<float, 3 - N, 1>::Zero();
        eval->set(p, i);
    }

    vertices.row(N) = eval->values(vertices.cols()).template cast<double>();
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

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
bool SimplexTree<N>::isInSimplex(unsigned corner,
                                 const std::array<int, N> simplex)
{
    bool included = true;
    for (unsigned a=0; a < simplex.size(); ++a)
    {
        switch (simplex[a])
        {
            case  0:    break;
            case -1:    included &= (corner & (1 << a)) == 0; break;
            case  1:    included &= (corner & (1 << a)) != 0; break;
            default:    assert(false); break;
        }
    }
    return included;
}

}   // namespace Kernel
