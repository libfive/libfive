/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/indexes.hpp"

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
#include <iostream>
#endif

namespace libfive {

/*
 *  This is documented in a long article at
 *  https://mattkeeter.com/projects/qef
 */
template <unsigned N>
class QEF
{
public:
    struct Solution {
        Eigen::Matrix<double, N, 1> position;
        Eigen::Matrix<bool, N, 1> constrained;
        double value;
        unsigned rank;
        double error;
    };

    using Matrix = Eigen::Matrix<double, N + 1, N + 1>;
    using RowVector = Eigen::Matrix<double, 1, N + 1>;
    using Vector = Eigen::Matrix<double, N + 1, 1>;

    QEF() : AtA(Matrix::Zero()), AtBp(Matrix::Zero()), BptBp(Matrix::Zero())
    {
        // Nothing to do here
    }

    /*
     *  Accumulate QEFs by summing
     */
    QEF& operator+=(const QEF& other) {
        AtA += other.AtA;
        AtBp += other.AtBp;
        BptBp += other.BptBp;

        return *this;
    }

    QEF& operator/=(const double& other) {
        AtA /= other;
        AtBp /= other;
        BptBp /= other;

        return *this;
    }

    void reset() {
        AtA = Matrix::Zero();
        AtBp = Matrix::Zero();
        BptBp = Matrix::Zero();
    }

    /*  Inserts a new sample into the QEF.  If the normal has
     *  non-finite values, it's replaced with an all-zeros normal
     *  before insertion. */
    void insert(Eigen::Matrix<double, 1, N> position,
                Eigen::Matrix<double, 1, N> normal,
                double value)
    {
        if (!normal.array().isFinite().all()) {
            normal.array() = 0.0;
        }
        RowVector ni;
        RowVector pi;

        ni << normal, -1;
        pi << position, value;

        RowVector Bp_row = ni.cwiseProduct(pi);

        AtA += ni.transpose() * ni;
        AtBp += ni.transpose() * Bp_row;
        BptBp += Bp_row.transpose() * Bp_row;
    }

    Solution solve(Eigen::Matrix<double, 1, N> target_pos=
                        Eigen::Matrix<double, 1, N>::Zero(),
                   double target_value=0.0) const
    {
        Vector target;
        target << target_pos.transpose(), target_value;

        const auto AtB_ = AtB();
        // Unpack from raw solution to position + value form
        const auto sol = QEF<N>::solve(AtA, AtB_, target);

        Solution out;
        out.position = sol.value.template head<N>();
        out.value = sol.value(N);
        out.rank = sol.rank - 1; // Skip the rank due to value
        out.constrained.array() = false;

        // Calculate the resulting error, hard-code the matrix size here so
        // that Eigen checks that all of our types are correct.
        Eigen::Matrix<double, 1, 1> err =
            sol.value.transpose() * AtA * sol.value
            - 2 * sol.value.transpose() * AtB_
            + BtB();
        out.error = err(0);

        return out;
    }

    /*
     *  Returns a new QEF with the axes set in a bitfield mask preserved
     *
     *  For example, this lets you go from a 2D QEF to a 1D QEF by dropping
     *  one axis from the matrices.
     */
    template <unsigned mask>
    QEF<bitcount(mask)> sub() const
    {
        static_assert(bitcount(mask) <= N, "Too many axes");

        QEF<bitcount(mask)> out;
        unsigned row_ = 0;
        for (unsigned row=0; row <= N; ++row) {
            unsigned col_ = 0;
            if ((mask & (1 << row)) || row == N) {
                for (unsigned col=0; col <= N; ++col) {
                    if ((mask & (1 << col)) || col == N) {
                        out.AtA(row_, col_) = AtA(row, col);
                        out.AtBp(row_, col_) = AtBp(row, col);
                        out.BptBp(row_, col_) = BptBp(row, col);
                        col_++;
                    }
                }
                assert(col_ == bitcount(mask) + 1);
                row_++;
            }
        }
        assert(row_ == bitcount(mask) + 1);

        return out;
    }

    /*
     *  Solves this QEF with constraints applied so that the solution lies
     *  in the subspace specified by Neighbor_
     *
     *  Constrained axes are given values based on the evaluation region.
     *
     *  For example, you could use this function to solve a 2D QEF constrained
     *  to lie on a specific cell edge.
     *
     *  This is a building block for more the more general task of "solve this
     *  QEF constrained to a particular region", where we iterate over
     *  subspaces in descending order of dimensionality until we find one
     *  which doesn't escape.
     */
    template <unsigned Neighbor_>
    Solution solveConstrained(Region<N> region,
                              Eigen::Matrix<double, 1, N> target_pos=
                                  Eigen::Matrix<double, 1, N>::Zero(),
                              double target_value=0.0) const
    {
        constexpr NeighborIndex Neighbor(Neighbor_);

        constexpr unsigned NumConstrainedAxes = N - Neighbor.dimension();
        static_assert(NumConstrainedAxes <= N,
                      "Wrong number of constrained axes");

        Eigen::Matrix<double, N + 1 - NumConstrainedAxes,
                              N + 1 - NumConstrainedAxes> AtA_c;
        Eigen::Matrix<double, N + 1 - NumConstrainedAxes, 1> AtB_c;
        Eigen::Matrix<double, N + 1 - NumConstrainedAxes, 1> target_c;

        // Cache the AtB calculation so we only do it once
        const auto AtB_ = AtB();

        /*
         *  This is a weird trick to do constrained matrix solving:
         *
         *  We recognize that we're solving (A^TA) x = (A^TB), with certain
         *  rows of x fixed.  We drop those fixed rows + columns from A^TA and
         *  A^TB, and subtract their value from A^TB to compensate.
         *
         *  A more detailed writeup is at
         *  mattkeeter.com/projects/qef/#alternate-constraints
         */
        unsigned r = 0;
        for (unsigned row=0; row < N + 1; ++row) {
            if (row == N || !Neighbor.isAxisFixed(row)) {
                AtB_c(r) = AtB_(row);
                target_c(r) = (row == N) ? target_value : target_pos(row);

                unsigned c = 0;
                for (unsigned col=0; col < N + 1; ++col) {
                    if (col == N || !Neighbor.isAxisFixed(col)) {
                        AtA_c(r, c) = AtA(row, col);
                        c++;
                    } else {
                        AtB_c(r) -= AtA(row, col) *
                            ((Neighbor.pos() & (1 << col))
                                ? region.upper(col)
                                : region.lower(col));
                    }
                }
                assert(c == N + 1 - NumConstrainedAxes);
                r++;
            }
        }
        assert(r == N + 1 - NumConstrainedAxes);

        auto sol = QEF<N - NumConstrainedAxes>::solve(
                AtA_c, AtB_c, target_c);

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
        std::cout << "Solving constrained: AtA = [\n" << AtA_c <<
            "]\nAtB = [\n" << AtB_c <<
            "]\nBtB = [\n" << BtB() <<
            "]\nTarget = [\n" << target_c << "\n";

        std::cout << "Got solution with pos: [\n" <<
            sol.value << "]\n rank = " << sol.rank << "\n";
#endif

        Solution out;
        r = 0;
        for (unsigned i=0; i < N; ++i) {
            if (Neighbor.isAxisFixed(i)) {
                out.position(i) = (Neighbor.pos() & (1 << i))
                    ? region.upper(i)
                    : region.lower(i);
                out.constrained(i) = true;
            } else {
                out.position(i) = sol.value(r++);
                out.constrained(i) = false;
            }
        }
        out.value = sol.value(r);
        out.rank = sol.rank + NumConstrainedAxes;

        // Calculate the resulting error, hard-coding the matrix size here so
        // that Eigen checks that all of our types are correct.
        // This error calculation uses the unconstrained matrices to return
        // a true error, rather than a weird value that could be < 0.
        Vector v;
        v << out.position, out.value;
        Eigen::Matrix<double, 1, 1> err =
            v.transpose() * AtA * v -
            2 * v.transpose() * AtB_ +
            BtB();
        out.error = err(0);

        return out;
    }

    /*
     *  Solves the equivalent DC QEF, which tries to find the point
     *  where all of the planes intersect at a distance-field value of 0
     */
    Solution solveDC(Eigen::Matrix<double, 1, N> target_pos=
                         Eigen::Matrix<double, 1, N>::Zero()) const;

    /*  Returns the rank of the DC matrix subset.
     *
     *  This assumes that the normals have been normalized, because it uses an
     *  absolute threshold to decide when to discard eigenvalues. */
    unsigned rankDC() const
    {
        // Pick out the DC subset of the matrix
        Eigen::Matrix<double, N, N> AtA_c = AtA.template topLeftCorner<N, N>();

        // Use the same logic as `static RawSolution solve()`, but
        // only accumulate the rank with an absolute threshold.
        Eigen::SelfAdjointEigenSolver<Eigen::Matrix<double, N, N>> es(AtA_c);
        auto eigenvalues = es.eigenvalues().real();

        unsigned rank = 0;
        for (unsigned i=0; i < N; ++i) {
            if (fabs(eigenvalues[i]) > 0.1) {
                rank++;
            }
        }
        return rank;
    }

    /*
     *  Returns the direction of travel which produces the minimum QEF error.
     *
     *  For a DC QEF that's on an edge, this is equivalent to the edge's
     *  direction, which is useful for sliding vertices into their cells.
     */
    Eigen::Matrix<double, N, 1> slideDC() const;

    /*
     *  A bit of magic matrix math to extract the distance value
     */
    double averageDistanceValue() const {
        return AtBp(N, N) / AtA(N, N);
    }

    /*
     *  Solves the given QEF, bounded to lie within the given region.
     *
     *  By default, minimizes towards the center of the region and
     *  our average value.  Call the full-argument version of the function
     *  to minimize towards a different point.
     *
     *  This is implemented by walking down in dimensionality from N to 0,
     *  picking the lowest-error solution available that is within the bounds.
     */
    Solution solveBounded(const Region<N>& region, double shrink=(1 - 1e-9))
    {
        return solveBounded(region, shrink,
                            (region.lower + region.upper) / 2.0,
                            AtBp(N, N) / AtA(N, N));
    }

    Solution solveBounded(const Region<N>& region, double shrink,
                          Eigen::Matrix<double, 1, N> target_pos,
                          double target_value) const
    {
        const auto region_ = region.shrink(shrink);

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
        std::cout << "------------------------------------------------------\n";
        std::cout << "Solving bounded between \n[" << region.lower.transpose() << "]\n["
            << region.upper.transpose() << "]\n";
        std::cout << "shrunk region: \n[" << region_.lower.transpose() << "]\n["
            << region_.upper.transpose() << "]\n";
#endif

        {   //  First, check the full-dimension, unconstrained solver
            auto sol = solve(target_pos, target_value);
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
            std::cout << "Got solution at [" << sol.position.transpose() << "]\n";
#endif
            if (region_.contains(sol.position, 0)) {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "Got full-dimension solution\n";
#endif
                return sol;
            }
        }

        // Construct an empty solution object with infinite error
        // (and dummy values for other fields)
        Solution out = {
            Eigen::Matrix<double, N, 1>::Zero(), /* positions */
            Eigen::Matrix<bool, N, 1>::Zero(), /* constrained */
            std::nan(""), /* value */
            0, /* rank */
            std::numeric_limits<double>::infinity()}; /* error */

        // Do static loop unrolling to check every smaller dimension
        // (e.g. for a cell, check every face, then every edge, then
        //  every corner, picking the first case where the QEF solution
        //  doesn't escape the bounds).
        UnrollDimension<(int)N - 1>()(
                *this, region_, target_pos, target_value, out);

        assert(!std::isinf(out.error));
        return out;
    }

    /*  Calculates the QEF error for a given position + value. */
    double error(const Eigen::Matrix<double, N, 1>& pos,
                 const double value) const {
        Vector v;
        v << pos, value;
        Eigen::Matrix<double, 1, 1> err =
            v.transpose() * AtA * v -
            2 * v.transpose() * AtB() +
            BtB();
        return err(0);
    }

    /*  Calculates the QEF error for a given position, with value allowed
     *  to float and minimize the error.  This is equivalent to doing a
     *  constrained solve with the position fixed. */
    Solution minimizeErrorAt(const Eigen::Matrix<double, N, 1>& pos) const {
        // This is based on solveConstrained above, specialized
        // to constrain all of the position axes and let the
        // value axis float.
        Eigen::Matrix<double, 1, 1> AtA_c;
        Eigen::Matrix<double, 1, 1> AtB_c;
        Eigen::Matrix<double, 1, 1> target_c;

        // Cache the AtB calculation so we only do it once
        const auto AtB_ = AtB();

        AtB_c(0) = AtB_(N);
        AtA_c(0, 0) = AtA(N, N);
        target_c(0) = 0.0;
        for (unsigned col=0; col < N; ++col) {
            AtB_c(0) -= AtA(N, col) * pos(col);
        }

        // TODO:  This is probably reducible to a single division, since
        // it's all one-coefficient arrays.
        auto sol = QEF<0>::solve(AtA_c, AtB_c, target_c);

        Vector v;
        v << pos, sol.value(0);
        Eigen::Matrix<double, 1, 1> err =
            v.transpose() * AtA * v -
            2 * v.transpose() * AtB_ +
            BtB();

        Solution out;
        out.position = pos;
        out.constrained.array() = true;
        out.value = sol.value(0);
        out.rank = 0;
        out.error = err(0);
        return out;
    }

protected:
    /*
     *  Unrolls constrained solving along dimensions
     *
     *  Dummy is a dummy parameter that prevents a warning about
     *  specialization in a class scope; it has no effect
     */
    template <int TargetDimension, unsigned Dummy=0>
    struct UnrollDimension {
        void operator()(
                const QEF<N>& qef, const Region<N>& region,
                const Eigen::Matrix<double, 1, N>& target_pos,
                double target_value, Solution& out)
        {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
            std::cout << "UnrollDimension<" << TargetDimension
                      << "> called with N = " << N <<"\n";
#endif

            UnrollSubspace<TargetDimension, ipow(3, N)>()(
                qef, region, target_pos, target_value, out);

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
            std::cout << "Done unrolling subspace\n";
#endif

            // Continue unrolling if we haven't found a bounded position,
            // moving to the next-lowest dimension (cell to face, face to edge,
            // edge to vertex, terminating at -1 dimensions)
            assert(!std::isinf(out.error));
            if (!region.contains(out.position, 0))
            {
                // Reset the error value, then try the next dimension down
                out.error = std::numeric_limits<double>::infinity();

                UnrollDimension<TargetDimension - 1, Dummy>()(
                        qef, region, target_pos, target_value, out);
            }
        }
    };

    // We go all the way down to 0D, then terminate static unrolling at -1D
    template <unsigned Dummy>
    struct UnrollDimension<-1, Dummy> {
        void operator()(const QEF<N>&, const Region<N>&,
                        const Eigen::Matrix<double, 1, N>&,
                        double, Solution&)
        {
            // Terminate static unrolling here
        }
    };

    template <unsigned TargetDimension, unsigned TargetSubspace>
    struct UnrollSubspace {
        void operator()(
                const QEF<N>& qef, const Region<N>& region,
                const Eigen::Matrix<double, 1, N>& target_pos,
                double target_value, Solution& out)
        {
            // If this neighbor is of the target dimension, then check for
            // an improved solution constrained to this neighbor.
            if (TargetDimension ==
                NeighborIndex(TargetSubspace - 1).dimension())
            {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "  Solving constrained to subspace " << TargetSubspace - 1 << "\n";
#endif
                // Calculate the constrained solution, including error
                const auto sol = qef.solveConstrained<TargetSubspace - 1>(
                        region, target_pos, target_value);

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "  Got solution at " << sol.position.transpose() << " and error " << sol.error << "\n";
#endif
                // If this solution is an improvement, then store it
                if (sol.error < out.error || (sol.error == out.error &&
                                              !region.contains(sol.position, 0) &&
                                               region.contains(out.position, 0)))
                {
                    assert(sol.error >= -1e-12);
                    out = sol;
                } else {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                    std::cout << "  Not an improvement; skipping\n";
#endif
                }
            }

            // Statically unroll the loop across all neighbors
            // (keeping the target dimension constant)
            UnrollSubspace<TargetDimension, TargetSubspace - 1>()(
                    qef, region, target_pos, target_value, out);
        }
    };

    // Terminates static unrolling across neighbors with a fixed dimension
    template <unsigned TargetDimension>
    struct UnrollSubspace<TargetDimension, 0> {
        void operator()(const QEF<N>&, const Region<N>&,
                        const Eigen::Matrix<double, 1, N>&, double, Solution&)
        {
            // Nothing to do here
        }
    };

    struct RawSolution {
        Vector value;
        unsigned rank;
    };

    /*
     *  Core QEF solver, which knows nothing of our fancy constraint
     *  systems or distance-field details.  It just takes the three matrices
     *  (and a point to minimize towards) and does the math.
     */
    static RawSolution solve(
            const Matrix& AtA,
            const Vector& AtB,
            const Vector& target,
            const double eigenvalue_cutoff_relative=1e-12,
            const double eigenvalue_cutoff_absolute=0)
    {
        // Our high-level goal here is to find the pseduo-inverse of AtA,
        // with special handling for when it isn't of full rank.
        Eigen::SelfAdjointEigenSolver<Matrix> es(AtA);
        auto eigenvalues = es.eigenvalues().real();

        // Build the SVD's diagonal matrix, truncating near-singular eigenvalues
        Matrix D = Matrix::Zero();
        const double max_eigenvalue = eigenvalues.cwiseAbs().maxCoeff();

        unsigned rank = 0;
        for (unsigned i=0; i < N + 1; ++i) {
            const auto e = fabs(eigenvalues[i]);
            if ((eigenvalue_cutoff_relative &&
                 e / max_eigenvalue > eigenvalue_cutoff_relative) ||
                (eigenvalue_cutoff_absolute &&
                 e > eigenvalue_cutoff_absolute))
            {
                D.diagonal()[i] = 1 / eigenvalues[i];
                rank++;
            }
        }

        // SVD matrices
        auto U = es.eigenvectors().real().eval(); // = V

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
        std::cout << "Eigenvalues: [\n" << eigenvalues << "]\n";
        std::cout << "Eigenvectors: [\n" << U << "]\n";
#endif

        // Pseudo-inverse of A
        auto AtA_inv = (U * D * U.transpose()).eval();

        // Solve for vertex position (minimizing distance to target)
        Vector sol = AtA_inv * (AtB - (AtA * target)) + target;

        // Unpack these results into our solution struct
        RawSolution out;
        out.value = sol;
        out.rank = rank;

        return out;
    }

    // Pack our expanded A^T B' into a convention A^T B for solving
    Vector AtB() const {
        return AtBp * Vector::Ones();
    }

    // Pack our expanded B'^T B' into a convention B^T B for solving
    Eigen::Matrix<double, 1, 1> BtB() const {
        return RowVector::Ones() * BptBp * Vector::Ones();
    }

    // Here are our matrices!
    Matrix AtA;
    Matrix AtBp;
    Matrix BptBp;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class QEF<0>;
    friend class QEF<1>;
    friend class QEF<2>;
    friend class QEF<3>;
};

template <>
inline typename QEF<0>::Solution QEF<0>::solveDC(Eigen::Matrix<double, 1, 0>) const
{
    Solution sol;
    sol.value = 0.0;
    sol.rank = 0;
    return sol;
}

template <unsigned N>
inline typename QEF<N>::Solution QEF<N>::solveDC(Eigen::Matrix<double, 1, N> target_pos) const
{
    // Cache the AtB calculation so we only do it once
    const auto AtB_ = AtB();

    Eigen::Matrix<double, N, N> AtA_c =
        AtA.template topLeftCorner<N, N>();
    Eigen::Matrix<double, N, 1> AtB_c =
        AtB_.template topRows<N>();
    Eigen::Matrix<double, N, 1> target_c =
        target_pos.transpose();

    auto sol = QEF<N - 1>::solve(
            AtA_c, AtB_c, target_c, 0, 0.1);

    Solution out;
    out.position = sol.value;
    out.value = 0.0;
    out.rank = sol.rank;

    // Calculate the resulting error, hard-coding the matrix size here so
    // that Eigen checks that all of our types are correct.
    // This error calculation uses the unconstrained matrices to return
    // a true error, rather than a weird value that could be < 0.
    Vector v;
    v << out.position, out.value;
    Eigen::Matrix<double, 1, 1> err =
        v.transpose() * AtA * v -
        2 * v.transpose() * AtB_ +
        BtB();
    out.error = err(0);

    return out;
}

template <>
inline Eigen::Matrix<double, 0, 1> QEF<0>::slideDC() const {
    return {};
}

template <unsigned N>
inline Eigen::Matrix<double, N, 1> QEF<N>::slideDC() const {
    // Pick out the DC subset of the matrix
    Eigen::Matrix<double, N, N> AtA_c = AtA.template topLeftCorner<N, N>();

    Eigen::SelfAdjointEigenSolver<Eigen::Matrix<double, N, N>> es(AtA_c);
    unsigned i;
    es.eigenvalues().minCoeff(&i);

    return es.eigenvectors().col(i);
}


}   // namespace libfive
