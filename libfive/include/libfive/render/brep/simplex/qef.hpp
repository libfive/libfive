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

namespace Kernel {

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

    void reset() {
        AtA = Matrix::Zero();
        AtBp = Matrix::Zero();
        BptBp = Matrix::Zero();
    }

    void insert(Eigen::Matrix<double, 1, N> position,
                Eigen::Matrix<double, 1, N> normal,
                double value)
    {
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
     *  in the subspace specified by NeighborIndex_
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

        constexpr unsigned MatrixSize = N + NumConstrainedAxes + 1;

        // We'll use a scale factor to avoid throwing the matrix rank
        // off.  For example adding a column that's [1 0 0 ...] when every
        // other matrix term is large will produce an eigenvalue that's
        // culled by the rank detection system.
        const double scale = AtA.cwiseAbs().mean();

        // Build AtA, with extra rows and columns for constraints
        Eigen::Matrix<double, MatrixSize, MatrixSize> AtA_c =
            Eigen::Matrix<double, MatrixSize, MatrixSize>::Zero();
        AtA_c.template topLeftCorner<N + 1, N + 1>() = 2 * AtA;

        // The constrained matrices are like AtA and AtB, plus
        // extra rows + columns to encode the constraints.
        Eigen::Matrix<double, MatrixSize, 1> AtB_c =
            Eigen::Matrix<double, MatrixSize, 1>::Zero();
        const auto AtB_ = AtB();
        AtB_c.template head<N + 1>() = 2 * AtB_;

        // The new target matrix is padded with the constrained
        // rows and modified to target the constrained positions
        Eigen::Matrix<double, MatrixSize, 1> target_c =
            Eigen::Matrix<double, MatrixSize, 1>::Zero();
        target_c.template head<N>() = target_pos;
        target_c(N) = target_value;

        {
            unsigned col = 0;
            for (unsigned j=0; j < N; ++j) {
                if (Neighbor.fixed() & (1 << j)) {
                    // Add symmetric termps to AtA
                    AtA_c(j, N + 1 + col) = scale;
                    AtA_c(N + 1 + col, j) = scale;

                    const auto c = scale * ((Neighbor.pos() & (1 << j))
                        ? region.upper(j)
                        : region.lower(j));

                    AtB_c(N + 1 + col) = c;
                    target_c(j) = c;
                    target_c(N + 1 + col) = c;

                    col++;
                }
            }
            assert(col == NumConstrainedAxes);
        }

        auto sol = QEF<N + NumConstrainedAxes>::solve(
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
        out.position = sol.value.template head<N>();
        out.value = sol.value(N);
        out.rank = sol.rank - NumConstrainedAxes - 1;

        // Calculate the resulting error, hard-coding the matrix size here so
        // that Eigen checks that all of our types are correct.
        // This error calculation uses the unconstrained matrices to return
        // a true error, rather than a weird value that could be < 0.
        Vector v = sol.value.template head<N + 1>();
        Eigen::Matrix<double, 1, 1> err =
            v.transpose() * AtA * v -
            2 * v.transpose() * AtB_ +
            BtB();
        out.error = err(0);

        return out;
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
    Solution solveBounded(const Region<N>& region)
    {
        return solveBounded(region,
                            (region.lower + region.upper) / 2.0,
                            AtBp(N, N) / AtA(N, N));
    }

    Solution solveBounded(const Region<N>& region,
                          Eigen::Matrix<double, 1, N> target_pos,
                          double target_value) const
    {

#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
        std::cout << "------------------------------------------------------\n";
        std::cout << "Solving bounded between \n[" << region.lower << "]\n["
            << region.upper << "]\n";
#endif

        {   //  First, check the full-dimension, unconstrained solver
            auto sol = solve(target_pos, target_value);
            if (region.contains(sol.position)) {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "Got full-dimension solution\n";
#endif
                return sol;
            }
        }

        // Construct an empty solution object with infinite error
        Solution out;
        out.error = std::numeric_limits<double>::infinity();

        // Do static loop unrolling to check every smaller dimension
        // (e.g. for a cell, check every face, then every edge, then
        //  every corner, picking the first case where the QEF solution
        //  doesn't escape the bounds).
        UnrollDimension<(int)N - 1>()(
                *this, region, target_pos, target_value, out);

        assert(!std::isinf(out.error));
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
        void operator()(const QEF<N>& qef, const Region<N>& region,
                        const Eigen::Matrix<double, 1, N>& target_pos,
                        double target_value,
                        Solution& out)
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
            if (std::isinf(out.error))
            {
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
        void operator()(const QEF<N>& qef, const Region<N>& region,
                        const Eigen::Matrix<double, 1, N>& target_pos,
                        double target_value,
                        Solution& out)
        {
            // If this neighbor is of the target dimension, then check for
            // an improved solution constrained to this neighbor.
            if (TargetDimension ==
                NeighborIndex(TargetSubspace - 1).dimension())
            {
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
                std::cout << "  UnrollSubspace<" << TargetDimension << ", "
                          << TargetSubspace << "> activated\n";
#endif
                // Calculate the constrained solution, including error
                const auto sol = qef.solveConstrained<TargetSubspace - 1>(
                        region, target_pos, target_value);

                // If this solution is an improvement, then store it
                if (region.contains(sol.position) && out.error > sol.error) {
                    assert(sol.error >= -1e-12);
                    out = sol;
                }
            }
#ifdef LIBFIVE_VERBOSE_QEF_DEBUG
            else {
                std::cout << "  UnrollSubspace<" << TargetDimension << ", "
                          << TargetSubspace << "> skipped\n";
            }
#endif

            // Statically unroll a loop, dropping in dimensionality loop
            UnrollSubspace<TargetDimension, TargetSubspace - 1>()(
                    qef, region, target_pos, target_value, out);
        }
    };

    // Terminates static unrolling
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
            const Vector& target)
    {
        // Our high-level goal here is to find the pseduo-inverse of AtA,
        // with special handling for when it isn't of full rank.
        Eigen::SelfAdjointEigenSolver<Matrix> es(AtA);
        auto eigenvalues = es.eigenvalues().real();

        // Build the SVD's diagonal matrix, truncating near-singular eigenvalues
        Matrix D = Matrix::Zero();
        const double max_eigenvalue = eigenvalues.cwiseAbs().maxCoeff();

        const double EIGENVALUE_CUTOFF = 1e-12;
        unsigned rank = 0;
        for (unsigned i=0; i < N + 1; ++i) {
            if (fabs(eigenvalues[i]) / max_eigenvalue > EIGENVALUE_CUTOFF) {
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

    friend class QEF<0>;
    friend class QEF<1>;
    friend class QEF<2>;
    friend class QEF<3>;
};

}   // namespace Kernel
