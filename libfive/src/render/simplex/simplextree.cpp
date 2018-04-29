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
#include "libfive/render/simplex/simplex.hpp"
#include "libfive/render/brep/eval_xtree.hpp"

namespace Kernel {

template <unsigned N>
SimplexTree<N>::SimplexTree(
        XTreeEvaluator* eval,
        Region<N> region,
        double min_feature, double max_err)
{
    // Early exit based on interval evaluation
    type = Interval::state(
            eval->interval.evalAndPush(
                region.lower3().template cast<float>(),
                region.upper3().template cast<float>()));
    switch (type)
    {
        case Interval::EMPTY:       // fallthrough
        case Interval::FILLED:      eval->interval.pop(); return;
        case Interval::AMBIGUOUS:   break;
        case Interval::UNKNOWN:     assert(false); break;
    }

    // Top-down construction: we recurse down to a minimum size
    if (((region.upper - region.lower) > min_feature).any())
    {
        recurse(eval, region, min_feature, max_err);
        eval->interval.pop();
        return;
    }

    // Store values for the cell corner evaluation
    for (unsigned i=0; i < children.size(); ++i)
    {
        Eigen::Vector3f p;
        p << region.corner(i).template cast<float>(),
             region.perp.template cast<float>();
        eval->array.set(p, i);
    }

    // Find the values + derivatives
    auto ds = eval->array.derivs(children.size());

    // Total QEF error (used to decide whether to recurse
    std::array<double, ipow(3, N)> errors;

    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        const auto t = Simplex<N>(i);

        // Find the shape of the A matrix
        const unsigned cols = t.freeAxes();
        const unsigned rows = pow(2, cols);

        //  The A matrix is of the form
        //  [n1x, n1y, n1z, -1]
        //  [n2x, n2y, n2z, -1]
        //  [n3x, n3y, n3z, -1]
        //  ...
        //  [1, 0, 0, 0]
        //  [0, 1, 0, 0]
        //  [0, 0, 1, 0]
        //
        //  (with one row for each sampled point's normal, and one row
        //  for each active axis in this simple).
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> A(rows + cols, cols + 1);
        A.array() = 0.0;

        //  The b matrix is of the form
        //  [(p1, w1) . (n1, -1)]
        //  [(p2, w2) . (n2, -1)]
        //  [(p3, w3) . (n3, -1)]
        //  ...
        //  [x_center]
        //  [y_center]
        //  [z_center]
        //
        //  (with one row for each sampled point, and one row for each
        //  active axis in this simplex)
        Eigen::Matrix<double, Eigen::Dynamic, 1> b(rows + cols, 1);
        b.array() = 0.0;

        //  In order the construct the b matrix, we first construct the matrix
        //  [p1, w1]
        //  [p2, w2]
        //  [p3, w3]
        //  ...
        //  then take the average and bias it before multiplying product with A
        Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> b_(rows, cols + 1);

        // Write the appropriate corner values to the A and b_ matrices.
        // We loop over every corner, filter which ones are in this particular
        // simplex, and only store their values.
        unsigned r=0;
        for (unsigned j=0; j < children.size(); ++j)
        {
            if (t.containsCorner(j))
            {
                unsigned c = 0;
                for (unsigned a=0; a < N; ++a)
                {
                    if (t[a] == 0)
                    {
                        A(r, c) = ds(a, j);
                        b_(r, c) = region.corner(j)(a);
                        c++;
                    }
                }
                assert(c == cols);
                A(r, c) = -1;
                b_(r, c) = ds(3, j);
                r++;
            }
        }
        assert(r == rows);

        // Construct the b matrix as discussed above
        b.topRows(rows) = (b_.array() * A.topRows(rows).array())
            .matrix().rowwise().sum();

        // Add a few extra rows to A and b to bias the solution towards
        // the center of the cell, but with a much weaker strength than
        // the rest of the constraints.
        Eigen::Matrix<double, Eigen::Dynamic, 1> center(cols + 1, 1);
        center = b_.colwise().mean();
        for (unsigned a=0; a < N; ++a)
        {
            if (t[a] == 0)
            {
                const unsigned c = r - rows;
                A(r, c) = 1e-6;
                b(r) = center(c) * 1e-6;
                r++;
            }
        }
        assert(r == rows + cols);

        // Solve QEF here
        Eigen::Matrix<double, Eigen::Dynamic, 1> result =
            A.colPivHouseholderQr().solve(b);

        // Store the error, only considering the plane/normal constraints
        // (and ignoring any error due to the centering constraint)
        errors[i] = (A.topRows(rows) * result - b.topRows(rows)).squaredNorm();

        // Unpack the QEF solution into the vertex array
        unsigned c = 0;
        bool bounded = true;
        for (unsigned a=0; a < N; ++a)
        {
            switch (t[a])
            {
                case SIMPLEX_CORNER_SPANS:
                    vertices(a, i) = result(c);
                    bounded &= (vertices(a, i) > region.lower(a));
                    bounded &= (vertices(a, i) < region.upper(a));
                    c++;
                    break;
                case SIMPLEX_CORNER_LOWER:
                    vertices(a, i) = region.lower(a);
                    break;
                case SIMPLEX_CORNER_UPPER:
                    vertices(a, i) = region.upper(a);
                    break;
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
                if (t.containsSimplex(j))
                {
                    // Unpack the relevant axes
                    unsigned c=0;
                    for (unsigned a=0; a < N; ++a)
                    {
                        if (t[a] == 0)
                        {
                            result(c++) = vertices(a, j);
                        }
                    }
                    double this_error =
                        (A.topRows(rows) * result - b.topRows(rows)).squaredNorm();

                    if (this_error < best_error)
                    {
                        vertices.col(i) = vertices.col(j);
                        best_error = this_error;
                    }
                }
            }
            errors[i] = best_error;
        }
    }

    // If the errors are too large, then recurse here
    if (std::accumulate(errors.begin(), errors.end(), 0.0) > max_err)
    {
        recurse(eval, region, min_feature, max_err);
        eval->interval.pop();
        return;
    }

    // Otherwise, do a second evaluation pass to refine the values of F(p)
    // at the simplex corners, turning them from estimates to true evaluations.
    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        Eigen::Vector3f p;
        p << vertices.col(i).template head<N>().template cast<float>(),
             Eigen::Array<float, 3 - N, 1>::Zero();
        eval->array.set(p, i);
    }
    vertices.row(N) = eval->array.values(vertices.cols())
        .template cast<double>();

    // Then, tag this tree as filled, empty, or ambiguous based on simplex
    // values (using the feature evaluator to tie-break in ambiguous cases).
    for (unsigned v=0; v < inside.size(); ++v)
    {
        switch (Interval::state<double>(vertices(N, v)))
        {
            case Interval::EMPTY:   inside[v] = false; break;
            case Interval::FILLED:  inside[v] = true; break;
            case Interval::AMBIGUOUS:
            {
                Eigen::Vector3f vert3;
                vert3 << vertices.col(v)
                            .template topRows<N>()
                            .template cast<float>(),
                         region.perp.template cast<float>();
                inside[v] = eval->feature.isInside(vert3);
                break;
            }
            case Interval::UNKNOWN: assert(false); break;
        }
    }

    bool all_filled = true;
    bool all_empty = true;
    for (auto& b : inside)
    {
        all_filled &=  b;
        all_empty  &= !b;
    }
    type = all_filled ? Interval::FILLED :
           all_empty  ? Interval::EMPTY  : Interval::AMBIGUOUS;

    eval->interval.pop();
}

template <unsigned N>
void SimplexTree<N>::recurse(
        XTreeEvaluator* eval, Region<N> region,
        double min_feature, double max_err)
{
    auto rs = region.subdivide();
    for (unsigned i=0; i < children.size(); ++i)
    {
        children[i].reset(
                new SimplexTree<N>(eval, rs[i], min_feature, max_err));
    }

    bool all_filled = true;
    bool all_empty = true;
    for (const auto& c : children)
    {
        all_filled &= (c->type == Interval::FILLED);
        all_empty  &= (c->type == Interval::EMPTY);
    }
    type = all_filled ? Interval::FILLED :
           all_empty  ? Interval::EMPTY  : Interval::AMBIGUOUS;

    if (type != Interval::AMBIGUOUS)
    {
        for (auto& c : children)
        {
            c.reset();
        }
    }
}

}   // namespace Kernel
