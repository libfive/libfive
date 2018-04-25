/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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

namespace Kernel {

template <int Rows, int Cols>
Eigen::Matrix<double, Cols, 1> solveQEF(
        const Eigen::Matrix<double, Rows, Cols>& A,
        const Eigen::Matrix<double, Rows, 1>& b)
{
    assert(A.rows() == b.rows());

    Eigen::EigenSolver<Eigen::Matrix<double, Rows, Cols>> es(A);

    // We use the eigenvalue decomposition to find the pseudo-inverse of A
    auto eigenvalues = es.eigenvalues().real();

    // Truncate near-singular eigenvalues in the SVD's diagonal matrix
    Eigen::Matrix<double, Cols, Cols> D = Eigen::Matrix<double, Cols, Cols>::Zero(A.cols(), A.cols());
    for (int i=0; i < Cols; ++i)
    {
        D.diagonal()[i] = (fabs(eigenvalues[i]) < 1e-3)
            ? 0 : (1 / eigenvalues[i]);
    }

    // SVD matrices
    auto U = es.eigenvectors().real().eval(); // = V

    // Pseudo-inverse of A
    auto A_ = (U * D * U.transpose()).eval();

    // Solve for vertex position
    return A_ * b;
}

}
