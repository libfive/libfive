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

namespace Kernel {
namespace SimplexSolver {

constexpr unsigned simplexDimension(unsigned simplex_number)
{
    return simplex_number
        ? ((simplex_number % 3) == 2) + simplexDimension(simplex_number / 3)
        : 0;
}

constexpr bool simplexContains(unsigned simplex_number,
                               unsigned corner_number)
{
    return simplex_number
        ? ((simplex_number % 3) == 2 ||
           (simplex_number % 3) == (corner_number & 1))
            && simplexContains(simplex_number / 3, corner_number >> 1)
        : (corner_number == 0);
}

constexpr unsigned cornerToSimplex(unsigned corner_index)
{
    return corner_index
        ? 3 * cornerToSimplex(corner_index >> 1) + (corner_index & 1)
        : 0;
}

constexpr unsigned simplexUnion(unsigned a, unsigned b)
{
    return (a || b)
        ? 3 * simplexUnion(a / 3, b / 3) +
            ((a % 3) != (b % 3) ? 2 : (a % 3))
        : 0;
}

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
template <unsigned BaseDimension, unsigned SimplexNumber>
Eigen::Matrix<double, BaseDimension, 1>
findVertex(const CornerArray<BaseDimension>& corners,
                Region<BaseDimension> region)
{
    constexpr unsigned SimplexDimension = simplexDimension(SimplexNumber);

    unsigned samples = 0;
    for (unsigned i=0; i < corners.size(); ++i) {
        if (simplexContains(SimplexNumber, i)) {
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
    Eigen::Matrix<double, Eigen::Dynamic, SimplexDimension + 1>
        A(samples, SimplexDimension + 1);

    // Store the center in space + evaluation result
    Eigen::Matrix<double, BaseDimension + 1, 1> center =
        Eigen::Matrix<double, BaseDimension + 1, 1>::Zero();
    int center_count = 0;

    //  The b matrix is of the form
    //  [(p1, w1) . (n1, -1)]
    //  [(p2, w2) . (n2, -1)]
    //  [(p3, w3) . (n3, -1)]
    //  ...
    //
    //  (with one row for each sampled point)
    Eigen::Matrix<double, Eigen::Dynamic, 1>
        b(samples, 1);

    // Load every sample into the A and b matrices
    unsigned sample = 0;
    for (unsigned i=0; i < corners.size(); ++i)
    {
        if (!simplexContains(SimplexNumber, i)) {
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
        unpack<SimplexNumber>(region.corner(i), pos);
        pos(SimplexDimension) = corners[i].value;

        // Unpack each intersection, which share pos
        for (auto& d : corners[i].deriv)
        {
            Eigen::Matrix<double, 1, SimplexDimension> row;
            unpack<SimplexNumber>(d, row);
            A.row(sample) << row, -1;

            b(sample) = A.row(sample) * pos;
            sample++;
        }
    }

    // TODO: Right now, we punt and simply return the center
    center /= center_count;
    return center.template head<BaseDimension>();
};


}   // namespace SimplexSolver
}   // namespace Kernel
