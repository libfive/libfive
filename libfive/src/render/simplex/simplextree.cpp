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

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
constexpr unsigned Cols(unsigned index)
{
    return Simplex<N>::freeAxesFromIndex(index);
}

template <unsigned N>
constexpr unsigned Rows(unsigned index)
{
    return ipow(2, Cols<N>(index));
}

template <unsigned N, unsigned index>
using A_matrix = Eigen::Matrix<double, Rows<N>(index), Cols<N>(index) + 1>;

template <unsigned N, unsigned index>
using B_matrix = Eigen::Matrix<double, Rows<N>(index), 1>;
template <unsigned N, unsigned index>
using Center = Eigen::Matrix<double, Cols<N>(index), 1>;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N, unsigned index, unsigned checking, unsigned dims>
struct VertexPositioner
{
    static void call(const A_matrix<N, index>& A,
                     const B_matrix<N, index>& b,
                     const Center<N, index>& center,
                     const Region<N>& region,
                     /*  Outputs */
                     bool& bounded, double& error,
                     Eigen::Matrix<double, N + 1, 1>& vert)
    {
        constexpr unsigned rows = Rows<N>(index);
        constexpr unsigned cols = Cols<N>(index);

        const auto t = Simplex<N>::fromIndex(index);
        const auto t_ = Simplex<N>::fromIndex(checking);
        if (dims == Simplex<N>::freeAxesFromIndex(checking) &&
            t.containsSimplex(t_))
        {
            constexpr unsigned cols_ = Cols<N>(checking);

            /*  The A_ matrix is a two-part modification of the A input:
             *  - Columns (axes) that are constrained in this subsimplex
             *    are removed, and their value is subtracted from b_
             *  - We add rows to the bottom of A_ and b_ to bias the solver
             *    towards the center of the cell. */
            Eigen::Matrix<double, rows + cols_, cols_ + 1> A_;
            Eigen::Matrix<double, rows + cols_, 1> b_;

            const double CENTERING_WEIGHT = 1e-6;

            //  Initialize both arrays to 0, to begin
            A_.array() = 0.0;
            b_.array() = 0.0;

            // Initialize b_; we'll modify it later on.
            b_.template topRows<rows>() = b;

            // Unpack the relevant axes
            unsigned c=0;
            unsigned c_=0;
            for (unsigned a=0; a < N; ++a)
            {
                // Only pay attention to axes that are spanning the parent
                if (t[a] != SIMPLEX_CORNER_SPANS)
                {
                    continue;
                }

                switch (t_[a])
                {
                    // If this axis is also spanning in the child, then
                    // copy the whole column over and add the centering
                    // constraint.
                    case SIMPLEX_CORNER_SPANS:
                        A_.template topRows<rows>().col(c_) = A.col(c);
                        A_(rows + c_, c_) = CENTERING_WEIGHT;
                        b_(rows + c_) = center(c) * CENTERING_WEIGHT;
                        c_++;
                        break;
                    // If this axis is spanning in the parent simplex
                    // but not spanning in the child simplex, then apply
                    // an offset to the new b_ matrix
                    case SIMPLEX_CORNER_LOWER:
                        b_.template topRows<rows>() +=
                            A.col(c) * region.lower(a); break;
                    case SIMPLEX_CORNER_UPPER:
                        b_.template  topRows<rows>() +=
                            A.col(c) * region.upper(a); break;
                }
                c++;
            }
            assert(c_ == cols_);
            assert(c == cols);
            A_.template topRows<rows>().col(cols_).array() = -1.0;

            // Solve QEF here
            Eigen::Matrix<double, cols_ + 1, 1> result_ =
                A_.colPivHouseholderQr().solve(b_);

            // Reinflate into a vertex that fits the parent simplex
            Eigen::Matrix<double, cols + 1, 1> result;
            c = 0;
            c_ = 0;
            for (unsigned a=0; a < N; ++a)
            {
                // Only pay attention to axes that are spanning the parent
                if (t[a] != SIMPLEX_CORNER_SPANS)
                {
                    continue;
                }

                switch (t_[a])
                {
                    case SIMPLEX_CORNER_SPANS:
                        result(c) = result_(c_++); break;
                    case SIMPLEX_CORNER_LOWER:
                        result(c) = region.lower(a); break;
                    case SIMPLEX_CORNER_UPPER:
                        result(c) = region.upper(a); break;
                }
                c++;
            }
            assert(c == cols);
            assert(c_ == cols_);

            // Copy over the solution's distance-field value.
            result(c) = result_(c_);

            // Calculate error given the original QEF
            double this_error = (A * result - b).squaredNorm();

            // If the error is less than our best-case result so far,
            // then inflate all the way up to normal vertex size
            // and save the vertex if it is bounded.
            if (this_error < error)
            {
                Eigen::Matrix<double, N + 1, 1> this_vert;
                c=0;
                bool this_bounded = true;
                for (unsigned a=0; a < N; ++a)
                {
                    switch (t[a])
                    {
                        case SIMPLEX_CORNER_SPANS:
                            this_vert(a) = result(c++);
                            this_bounded &= (this_vert(a) >= region.lower(a));
                            this_bounded &= (this_vert(a) <= region.upper(a));
                            break;
                        case SIMPLEX_CORNER_LOWER:
                            this_vert(a) = region.lower(a);
                            break;
                        case SIMPLEX_CORNER_UPPER:
                            this_vert(a) = region.upper(a);
                            break;
                    }
                }
                assert(c == result.rows() - 1);
                this_vert(N) = result(c);
                if (this_bounded)
                {
                    vert = this_vert;
                    error = this_error;
                    bounded = true;
                }
            }
        }
        // Continue unrolling
        VertexPositioner<N, index, checking + 1, dims>::call(
                A, b, center, region, bounded, error, vert);
    }
};

/*
 *  If we've reached past the last simplex and are now bounded, then
 *  stop looping; otherwise, continue unrolling with one fewer dimension.
 */
template <unsigned N, unsigned index, unsigned dims>
struct VertexPositioner<N, index, ipow(3, N), dims>
{
    static void call(const A_matrix<N, index>& A,
                     const B_matrix<N, index>& b,
                     const Center<N, index>& center,
                     const Region<N>& region,
                     /* Outputs */
                     bool& bounded, double& error,
                     Eigen::Matrix<double, N + 1, 1>& vert)
    {
        if (!bounded)
        {
            VertexPositioner<N, index, 0, dims - 1>::call(
                    A, b, center, region, bounded, error, vert);
        }
    }
};

/*
 *  The second termination condition:
 *  If we've reached past the last simplex at a dimensionality of 0,
 *  then we must be done unrolling forever.
 */
template <unsigned N, unsigned index>
struct VertexPositioner<N, index, ipow(3, N), 0>
{
    static void call(const A_matrix<N, index>&,
                     const B_matrix<N, index>&,
                     const Center<N, index>&,
                     const Region<N>&,
                     /* Outputs */
                     bool& bounded, double&,
                     Eigen::Matrix<double, N + 1, 1>&)
    {
        // Terminate unrolling with an assertion that we found at
        // least one acceptable vertex.
        (void)bounded;
        assert(bounded);
    }
};

template <unsigned N, unsigned index>
std::pair<Eigen::Matrix<double, N + 1, 1>, double> positionVertex(
        const A_matrix<N, index>& A,
        const B_matrix<N, index>& b,
        const Center<N, index>& center,
        const Region<N>& region)
{
    bool bounded = false;
    double error = std::numeric_limits<double>::infinity();
    Eigen::Matrix<double, N + 1, 1> vert;
    VertexPositioner<N, index, 0, Cols<N>(index)>::call(
            A, b, center, region, bounded, error, vert);
    return std::make_pair(vert, error);
}

////////////////////////////////////////////////////////////////////////////////

typedef Eigen::Array<float, 4, LIBFIVE_EVAL_ARRAY_SIZE> DerivArray;
typedef Eigen::Block<DerivArray, 4, Eigen::Dynamic> DerivArrayBlock;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N, unsigned index>  /* Number of dimensions */
struct Unroller
{
    static void call(DerivArrayBlock ds, const Region<N>& region,
                     /* Outputs */
                     Eigen::Matrix<double, N + 1, ipow(3, N)>& vertices,
                     std::array<double, ipow(3, N)>& errors)
    {
        constexpr unsigned cols = Simplex<N>::freeAxesFromIndex(index);
        constexpr unsigned rows = ipow(2, cols);

        //  The A matrix is of the form
        //  [n1x, n1y, n1z, -1]
        //  [n2x, n2y, n2z, -1]
        //  [n3x, n3y, n3z, -1]
        //  ...
        //
        //  (with one row for each sampled point's normal)
        Eigen::Matrix<double, rows, cols + 1> A;

        //  The b matrix is of the form
        //  [(p1, w1) . (n1, -1)]
        //  [(p2, w2) . (n2, -1)]
        //  [(p3, w3) . (n3, -1)]
        //  ...
        //
        //  (with one row for each sampled point)
        Eigen::Matrix<double, rows, 1> b;

        // We'll also keep track of the average position, which we'll
        // use to bias the solver later on.
        Eigen::Matrix<double, cols, 1> center;

        auto t = Simplex<N>::fromIndex(index);

        // Write the appropriate corner values to the A and b matrices.
        // We loop over every corner, filter which ones are in this particular
        // simplex, and only store their values.
        unsigned r=0;
        for (unsigned j=0; j < ipow(2, N); ++j)
        {
            if (!t.containsCorner(j))
            {
                continue;
            }

            Eigen::VectorXd p(cols + 1, 1);
            unsigned c = 0;
            for (unsigned a=0; a < N; ++a)
            {
                if (t[a] == SIMPLEX_CORNER_SPANS)
                {
                    A(r, c) = ds(a, j);
                    p(c) = region.corner(j)(a);
                    c++;
                }
            }
            assert(c == cols);
            p(c) = ds(3, j);

            A(r, c) = -1;
            b(r) = A.row(r) * p;
            center += p.head(cols);
            r++;
        }
        assert(r == rows);
        center /= rows;

        auto out = positionVertex<N, index>(A, b, center, region);
        vertices.col(index) = out.first;
        errors[index] = out.second;

        // And keep looping
        Unroller<N, index + 1>::call(ds, region, vertices, errors);
    }
};

template <unsigned N>  /* Number of dimensions */
struct Unroller<N, ipow(3, N)>
{
    static void call(DerivArrayBlock, const Region<N>&,
                     Eigen::Matrix<double, N + 1, ipow(3, N)>&,
                     std::array<double, ipow(3, N)>&)
    {
        // Terminates static unrolling
    }
};

template <unsigned N>  /* Number of dimensions */
void unrollLoop(DerivArrayBlock derivs, const Region<N>& region,
                /* outputs */
                Eigen::Matrix<double, N + 1, ipow(3, N)>& vertices,
                std::array<double, ipow(3, N)>& errors)
{
    Unroller<N, 0>::call(derivs, region, vertices, errors);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
SimplexTree<N>::SimplexTree(XTreeEvaluator* eval, Region<N> region,
                            double max_feature, double min_feature,
                            double max_err)
    : SimplexTree(eval, region, 0, max_feature, min_feature, max_err)
{
    // Nothing to do here (delegating constructor)
}

template <unsigned N>
SimplexTree<N>::SimplexTree(
        XTreeEvaluator* eval, Region<N> region, unsigned depth,
        double max_feature, double min_feature, double max_err)
    : depth(depth)
{
    vertices.array()  = 0.0/0.0;

    // Early exit based on interval evaluation
    type = Interval::state(
            eval->interval.evalAndPush(
                region.lower3().template cast<float>(),
                region.upper3().template cast<float>()));

    // Top-down construction: we recurse down to a minimum size
    if (((region.upper - region.lower) > max_feature).any())
    {
        recurse(eval, region, max_feature, min_feature, max_err);
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

    // Compile-time unrolled vertex positioner!
    unrollLoop<N>(ds, region, vertices, errors);

    // If the errors are too large, then recurse here
    if (((region.upper - region.lower) > min_feature).any() &&
        std::accumulate(errors.begin(), errors.end(), 0.0) > max_err)
    {
        recurse(eval, region, max_feature, min_feature, max_err);
        eval->interval.pop();
        return;
    }

    // Otherwise, do a second evaluation pass to refine the values of F(p)
    // at the simplex corners, turning them from estimates to true evaluations.
    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        Eigen::Vector3f p;
        p << vertices.col(i).template head<N>().template cast<float>(),
             region.perp.template cast<float>();
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
        double max_feature, double min_feature, double max_err)
{
    auto rs = region.subdivide();
    for (unsigned i=0; i < children.size(); ++i)
    {
        children[i].reset(new SimplexTree<N>(
                    eval, rs[i], depth + 1,
                    max_feature, min_feature, max_err));
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
        std::fill(inside.begin(), inside.end(), type == Interval::FILLED);
    }
}

}   // namespace Kernel
