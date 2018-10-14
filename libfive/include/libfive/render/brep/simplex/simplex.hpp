/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <cstdint>
#include <Eigen/Eigen>

namespace Kernel {

template <unsigned BaseDimension, unsigned SimplexDimension>
class Simplex
{
    /*
     *  Looks up this cell's corner mask, used in 3D for
     *  Marching Tetrahedrons and in 2D for Marching Triangles.
     */
    uint8_t cornerMask() const;

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        */
    unsigned rank;

    /*  Bitfield marking which corners are set */
    uint8_t corner_mask;

    /*  QEF matrices, for merging on the way up */
    Eigen::Matrix<double, SimplexDimension, SimplexDimension> AtA;
    Eigen::Matrix<double, SimplexDimension, 1> AtB;
    double BtB;

    /*  Solved vertex position */
    Eigen::Matrix<double, SimplexDimension, 1> vertex;

    /*  Perpendicular component of simplex position */
    Eigen::Matrix<double, BaseDimension - SimplexDimension, 1> perp;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel
