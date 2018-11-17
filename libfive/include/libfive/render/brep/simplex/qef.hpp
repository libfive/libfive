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

    QEF() :
        AtA(Eigen::Matrix<double, N + 1, N + 1>::Zero()),
        AtBp(Eigen::Matrix<double, N + 1, N + 1>::Zero()),
        BptBp(Eigen::Matrix<double, N + 1, N + 1>::Zero())
    {
        // Nothing to do here
    }

    void insert(Eigen::Matrix<double, 1, N> normal,
                Eigen::Matrix<double, 1, N> position,
                double value)
    {
        Eigen::Matrix<double, 1, N + 1> ni;
        Eigen::Matrix<double, 1, N + 1> pi;

        ni << normal, -1;
        pi << position, value;

        Eigen::Matrix<double, 1, N + 1> Bp_row;
            ni.cwiseProduct(pi);

        AtA += ni.transpose() * ni;
        AtBp += ni.transpose() * Bp_row;
        BptBp += Bp_row.transpose() * Bp_row;
    }

    Solution solve(const Eigen::Matrix<double, N + 1, 1> target=
                         Eigen::Matrix<double, N + 1, 1>::Zero())
    {
        // TODO: solve QEF here
        Eigen::Matrix<double, N + 1, 1> sol =
            Eigen::Matrix<double, N + 1, 1>::Zero();

        // We hard-code the matrix size here so that Eigen checks
        // that all of our types are correct.
        Eigen::Matrix<double, 1, 1> err =
            sol.transpose() * AtA * sol - 2 * sol.transpose() * AtB() + BtB();

        // Unpack these results into our solution struct
        Solution out;
        out.position = sol.template head<N>();
        out.value = sol(N);
        out.rank = 0;
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
