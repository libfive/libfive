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

#include "libfive/render/brep/simplex/corner.hpp"
#include "libfive/render/brep/simplex/corner.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/indexes.hpp"

namespace Kernel {

namespace SimplexSolver {

template <unsigned SimplexNumber, typename Input, typename Output>
void unpack(const Input& in, Output& out)
{
    for (unsigned s=SimplexNumber, index_in=0, index_out=0;
         s != 0; s /= 3, index_in++)
    {
        if ((s % 3) == 2) {
            out(index_out++) = in(index_in);
        }
    }
}

// Finds a vertex on the given simplex, clamping to lower-dimensional
// simplices if it tries to escape.
template <unsigned BaseDimension, unsigned _SimplexNumber>
Eigen::Matrix<double, BaseDimension, 1>
findVertex(const CornerArray<BaseDimension>& corners,
                Region<BaseDimension> region)
{
    constexpr NeighborIndex SimplexNumber(_SimplexNumber);
    constexpr unsigned SimplexDimension = SimplexNumber.dimension();

    unsigned samples = 0;
    for (unsigned i=0; i < corners.size(); ++i) {
        if (SimplexNumber.contains(CornerIndex(i))) {
            samples += corners[i].deriv.size();
        }
    }

    //  The A matrix is of the form
    //  [n1x, n1y, n1z, -1]
    //  [n2x, n2y, n2z, -1]
    //  [n3x, n3y, n3z, -1]
    //  ...
    //
    //  (with one row for each sampled point's normal)
    //
    //  The b matrix is of the form
    //  [(p1, w1) . (n1, -1)]
    //  [(p2, w2) . (n2, -1)]
    //  [(p3, w3) . (n3, -1)]
    //  ...
    //
    //  (with one row for each sampled point)
    //
    //  Instead of actually populating these matrices, we'll immediately
    //  construct the compact results AtA, AtB, BtB
    Eigen::Matrix<double, SimplexDimension + 1, SimplexDimension + 1> AtA;
    AtA.array() = 0.0;
    Eigen::Matrix<double, SimplexDimension + 1, 1> AtB;
    AtB.array() = 0.0;
    double BtB = 0.0;

    // We also store the center point as a n+1 dimensional point (where the
    // last dimension is the average distance-field value)
    Eigen::Matrix<double, BaseDimension + 1, 1> center =
        Eigen::Matrix<double, BaseDimension + 1, 1>::Zero();
    int center_count = 0;

    // Load every sample into the matrices
    for (unsigned i=0; i < corners.size(); ++i)
    {
        if (!SimplexNumber.contains(CornerIndex(i))) {
            continue;
        }

        // Store the average position
        decltype(center) c;
        c << region.corner(i), corners[i].value;
        center += c;
        center_count++;

        // pos is of the form [x, y, z, w], where w is the
        // distance field evaluated at the specific point.
        Eigen::Matrix<double, SimplexDimension + 1, 1> pos;
        unpack<SimplexNumber.i>(region.corner(i), pos);
        pos(SimplexDimension) = corners[i].value;

        // Unpack each intersection, which share pos
        // There's usually only one derivative per corner, but in some
        // cases (where the field is C1-discontinuous) there are more.
        for (const auto& deriv : corners[i].deriv)
        {
            // d is of the form [dx, dy, dz, -1]
            Eigen::Matrix<double, SimplexDimension + 1, 1> d;
            unpack<SimplexNumber.i>(deriv, d);
            d(SimplexDimension) = -1;

            AtA += d * d.transpose();
            const double b = d.dot(pos);
            AtB += d * b;
            BtB += b * b;
        }
    }

    // TODO: Right now, we punt and simply return the center
    center /= center_count;
    return center.template head<BaseDimension>();
};


}   // namespace SimplexSolver
}   // namespace Kernel
