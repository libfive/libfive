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
Eigen::VectorXd solveCenteredQEF(Eigen::MatrixXd A, Eigen::VectorXd b,
                                 Eigen::VectorXd center, double weight)
{
    assert(A.cols() >= center.rows());
    assert(A.rows() == b.rows());

    Eigen::MatrixXd A_(A.rows() + center.rows(), A.cols());
    A_.topRows(A.rows()) = A;
    A_.bottomLeftCorner(center.rows(), center.rows()) =
        Eigen::MatrixXd::Identity(center.rows(), center.rows()) * weight;
    A_.bottomRightCorner(center.rows(), A.cols() - center.rows()).array() = 0.0;

    Eigen::VectorXd b_(b.rows() + center.rows());
    b_.topRows(b.rows()) = b;
    b_.bottomRows(center.rows()) = center * weight;

    // Solve QEF here
    return A_.colPivHouseholderQr().solve(b_);
}

template <unsigned N  /* Number of dimensions */
         ,unsigned index  /* Index of the parent simplex */
          >
Eigen::Matrix<double, Simplex<N>::fromIndex(index).freeAxes() + 1, 1>
lolol(const Eigen::Matrix<double, ipow(Simplex<N>::fromIndex(index), 2),
                          Simplex<N>::fromIndex(index).freeAxes() + 1>& A,
      const Eigen::Matrix<double, ipow(Simplex<N>::fromIndex(index), 2), 1>& b)
{
    (void)A;
    (void) b;

}

////////////////////////////////////////////////////////////////////////////////

typedef Eigen::Array<float, 4, LIBFIVE_EVAL_ARRAY_SIZE> DerivArray;
typedef Eigen::Block<DerivArray, 4, Eigen::Dynamic> DerivArrayBlock;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
constexpr unsigned cols(unsigned index)
{
    return Simplex<N>::freeAxesFromIndex(index);
}

template <unsigned N>
constexpr unsigned rows(unsigned index)
{
    return ipow(2, cols<N>(index));
}

template <unsigned N, unsigned index>
using A_matrix = Eigen::Matrix<double, rows<N>(index), cols<N>(index) + 1>;

template <unsigned N, unsigned index>
using B_matrix = Eigen::Matrix<double, rows<N>(index), 1>;
template <unsigned N, unsigned index>
using Center = Eigen::Matrix<double, cols<N>(index), 1>;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N, unsigned index, unsigned checking, unsigned dims>
struct VertexPositioner
{
    static void call(const A_matrix<N, index>& A,
                     const B_matrix<N, index>& b,
                     const Center<N, index>& center,
                     const Region<N>& region, bool& bounded)
    {
        // Continue unrolling
        VertexPositioner<N, index, checking + 1, dims>::call(A, b, center, region, bounded);
    }
};

/*
 *  One termination condition for the unrolling:
 *  If we've reached past the last simplex and are still unbounded,
 *  then drop a dimension and restart the loop.
 */
template <unsigned N, unsigned index, unsigned dims>
struct VertexPositioner<N, index, ipow(3, N), dims>
{
    static void call(const A_matrix<N, index>& A,
                     const B_matrix<N, index>& b,
                     const Center<N, index>& center,
                     const Region<N>& region, bool& bounded)
    {
        VertexPositioner<N, index, 0, dims - 1>::call(A, b, center, region, bounded);
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
    static void call(const A_matrix<N, index>& A,
                     const B_matrix<N, index>& b,
                     const Center<N, index>& center,
                     const Region<N>& region, bool& bounded)
    {
        // Terminate
        (void)A;
        (void)b;
        (void)center;
        (void)region;
        assert(bounded);
    }
};

template <unsigned N, unsigned index>
void positionVertex(const A_matrix<N, index>& A,
                    const B_matrix<N, index>& b,
                    const Center<N, index>& center,
                    const Region<N>& region)
{
    bool bounded = false;
    VertexPositioner<N, index, 0, cols<N>(index)>::call(A, b, center, region, bounded);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N, unsigned index>  /* Number of dimensions */
struct Unroller
{
    static void call(DerivArrayBlock ds, const Region<N>& region,
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

        positionVertex<N, index>(A, b, center, region);

        // And keep looping
        Unroller<N, index + 1>::call(ds, region, errors);
    }
};

template <unsigned N>  /* Number of dimensions */
struct Unroller<N, ipow(N, 3)>
{
    static void call(DerivArrayBlock derivs, const Region<N>& region,
                     std::array<double, ipow(3, N)>& errors)
    {
        // Terminates static unrolling
        (void)derivs;
        (void)region;
        (void)errors;
    }
};

template <unsigned N>  /* Number of dimensions */
void unrollLoop(DerivArrayBlock derivs, const Region<N>& region,
                std::array<double, ipow(3, N)>& errors)
{
    Unroller<N, 0>::call(derivs, region, errors);
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

    unrollLoop<N>(ds, region, errors);

    for (unsigned i=0; i < vertices.cols(); ++i)
    {
        const auto t = Simplex<N>::fromIndex(i);

        // Find the shape of the A matrix
        const unsigned cols = t.freeAxes();
        const unsigned rows = pow(2, cols);

        //  The A matrix is of the form
        //  [n1x, n1y, n1z, -1]
        //  [n2x, n2y, n2z, -1]
        //  [n3x, n3y, n3z, -1]
        //  ...
        //
        //  (with one row for each sampled point's normal)
        Eigen::MatrixXd A(rows, cols + 1);

        //  The b matrix is of the form
        //  [(p1, w1) . (n1, -1)]
        //  [(p2, w2) . (n2, -1)]
        //  [(p3, w3) . (n3, -1)]
        //  ...
        //
        //  (with one row for each sampled point)
        Eigen::VectorXd b(rows, 1);

        // We'll also keep track of the average position, which we'll
        // use to bias the solver later on.
        Eigen::VectorXd center = Eigen::VectorXd::Zero(cols, 1);

        // Write the appropriate corner values to the A and b_ matrices.
        // We loop over every corner, filter which ones are in this particular
        // simplex, and only store their values.
        unsigned r=0;
        for (unsigned j=0; j < children.size(); ++j)
        {
            if (!t.containsCorner(j))
            {
                continue;
            }

            Eigen::VectorXd p(cols + 1, 1);
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
            p(c) = ds(3, j);

            A(r, c) = -1;
            b(r) = A.row(r) * p;
            center += p.head(cols);
            r++;
        }
        assert(r == rows);
        center /= rows;

        // Solve QEF here
        Eigen::VectorXd result = solveCenteredQEF<N>(A, b, center, 1e-6);
        assert(result.rows() == cols + 1);

        // Store the error
        errors[i] = (A * result - b).squaredNorm();

        // Then inflate all the way up to normal vertex size
        // and check to see whether the vertex is bounded.
        auto unpacked = unpack(t, result, region);
        bool bounded = unpacked.first;
        vertices.col(i) = unpacked.second;

        // If the result is outside the boundary, loop from 0 to i,
        // picking out sub-simplices, solving the same QEF (constrainted based
        // on the available free axes), and using the one with minimum error.
        if (!bounded)
        {
            errors[i] = std::numeric_limits<double>::infinity();
            for (unsigned j=0; j < i; ++j)
            {
                if (!t.containsSimplex(Simplex<N>::fromIndex(j)))
                {
                    continue;
                }

                Simplex<N> t_ = Simplex<N>::fromIndex(j);
                const unsigned cols_ = t_.freeAxes();

                // We cull axes from A_ based on which axes are free in the
                // sub-simplex t_, and apply the fixed change to b in b_.
                Eigen::MatrixXd A_(rows, cols_ + 1);
                Eigen::VectorXd b_(b);
                Eigen::VectorXd center_(cols_);

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
                        // copy the whole column over.
                        case SIMPLEX_CORNER_SPANS:
                            A_.col(c_) = A.col(c);
                            center_(c_) = center(c);
                            c_++;
                            break;
                        // If this axis is spanning in the parent simplex
                        // but not spanning in the child simplex, then apply
                        // an offset to the new b_ matrix
                        case SIMPLEX_CORNER_LOWER:
                            b_ += A.col(c) * region.lower(a); break;
                        case SIMPLEX_CORNER_UPPER:
                            b_ += A.col(c) * region.upper(a); break;
                    }
                    c++;
                }
                assert(c_ == cols_);
                assert(c == cols);
                A_.col(cols_).array() = -1.0;

                // Solve QEF here
                Eigen::VectorXd result_ = solveCenteredQEF<N>(
                        A_, b_, center_, 1e-6);
                assert(result_.rows() == cols_ + 1);

                // Reinflate into a vertex that fits the parent simplex
                Eigen::VectorXd result(cols + 1);
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

                // Then inflate all the way up to normal vertex size
                // and check to see whether the vertex is bounded.
                auto unpacked = unpack(t, result, region);
                bounded = unpacked.first;
                auto vert = unpacked.second;

                if (bounded && this_error < errors[i])
                {
                    vertices.col(i) = vert;
                    errors[i] = this_error;
                }
            }
        }
    }

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

template <unsigned N>
std::pair<bool, Eigen::Matrix<double, N + 1, 1>> SimplexTree<N>::unpack(
    const Simplex<N>& t, const Eigen::VectorXd& result,
    const Region<N>& region)
{
    Eigen::Matrix<double, N + 1, 1> vert;
    unsigned c=0;
    bool bounded = true;

    for (unsigned a=0; a < N; ++a)
    {
        switch (t[a])
        {
            case SIMPLEX_CORNER_SPANS:
                vert(a) = result(c++);
                bounded &= (vert(a) >= region.lower(a));
                bounded &= (vert(a) <= region.upper(a));
                break;
            case SIMPLEX_CORNER_LOWER:
                vert(a) = region.lower(a);
                break;
            case SIMPLEX_CORNER_UPPER:
                vert(a) = region.upper(a);
                break;
        }
    }
    assert(c == result.rows() - 1);
    vert(N) = result(c);
    return std::make_pair(bounded, vert);
}

}   // namespace Kernel
