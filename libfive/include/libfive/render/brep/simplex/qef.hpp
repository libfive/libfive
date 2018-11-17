/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

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
                   double target_value=0.0)
    {
        Vector target;
        target << target_pos.transpose(), target_value;

        // Our high-level goal here is to find the pseduo-inverse of AtA,
        // with special handling for when it isn't of full rank.
        Eigen::SelfAdjointEigenSolver<Matrix> es(AtA);
        auto eigenvalues = es.eigenvalues().real();

        // Build the SVD's diagonal matrix, truncating near-singular eigenvalues
        Matrix D = Matrix::Zero();
        const double max_eigenvalue = eigenvalues.cwiseAbs().maxCoeff();
        const double EIGENVALUE_CUTOFF = 0.02;
        unsigned rank = 0;
        for (unsigned i=0; i < N + 1; ++i) {
            if (fabs(eigenvalues[i]) / max_eigenvalue > EIGENVALUE_CUTOFF) {
                D.diagonal()[i] = 1 / eigenvalues[i];
                rank++;
            }
        }

        // SVD matrices
        auto U = es.eigenvectors().real().eval(); // = V

        // Pseudo-inverse of A
        auto AtA_inv = (U * D * U.transpose()).eval();

        // Solve for vertex position (minimizing distance to target)
        Vector sol = AtA_inv * (AtB() - (AtA * target)) + target;

        // Calculate the resulting error, hard-code the matrix size here so
        // that Eigen checks that all of our types are correct.
        Eigen::Matrix<double, 1, 1> err =
            sol.transpose() * AtA * sol - 2 * sol.transpose() * AtB() + BtB();

        // Unpack these results into our solution struct
        Solution out;
        out.position = sol.template head<N>();
        out.value = sol(N);
        out.rank = rank;
        out.error = err(0);

        return out;
    }

protected:
    Eigen::Matrix<double, N + 1, 1> AtB() const {
        return AtBp * Eigen::Matrix<double, N + 1, 1>::Ones();
    }

    Eigen::Matrix<double, 1, 1> BtB() const {
        return Eigen::Matrix<double, 1, N + 1>::Ones() *
               BptBp *
               Eigen::Matrix<double, N + 1, 1>::Ones();
    }

    Eigen::Matrix<double, N + 1, N + 1> AtA;
    Eigen::Matrix<double, N + 1, N + 1> AtBp;
    Eigen::Matrix<double, N + 1, N + 1> BptBp;
};

}   // namespace Kernel
