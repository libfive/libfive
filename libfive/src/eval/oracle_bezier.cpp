#include <iostream>
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

#include "libfive/eval/oracle_bezier.hpp"

using namespace Kernel;

BezierClosestPointOracle::BezierClosestPointOracle(const Eigen::Vector3f& a,
                                                   const Eigen::Vector3f& b,
                                                   const Eigen::Vector3f& c,
                                                   unsigned n)
    : a(a), b(b), c(c), lower(n, 3), upper(n, 3), lower_t(n, 1), upper_t(n, 1)
{
    for (unsigned i=0; i < n; ++i)
    {
        lower_t(i) = i / float(n);
        upper_t(i) = (i + 1) / float(n);
        lower.row(i) = at(lower_t(i));
        upper.row(i) = at(upper_t(i));
        std::cout << lower.row(i) << "\n" << upper.row(i) << "\n\n";
    }
}

void BezierClosestPointOracle::evalInterval(Interval::I& out)
{
    out = Interval::I(0, 1);
}

void BezierClosestPointOracle::evalPoint(float& out, size_t index)
{
    Eigen::Vector3f pt = points.col(index);

    float closest = std::numeric_limits<float>::infinity();
    for (unsigned i=0; i < lower.rows(); ++i)
    {
        const Eigen::Vector3f u = lower.row(i);
        const Eigen::Vector3f v = upper.row(i);
        const Eigen::Vector3f delta = v - u;
        const float len = delta.norm();

        // This is the projected distance of the interval
        // onto a particular line segment, as a scale from 0-to-1
        float proj = (pt - u).dot(delta) / pow(len, 2);

        float d_suggested = std::numeric_limits<float>::infinity();
        float t_suggested = std::nanf("");
        if (i == 0 && proj <= 0)
        {
            d_suggested = (pt - u).norm();
            t_suggested = 0;
        }
        else if (i == lower.rows() - 1 && proj >= 1)
        {
            d_suggested = (pt - v).norm();
            t_suggested = 1;
        }
        else if (proj >= 0 && proj <= 1)
        {
            d_suggested = (pt - (u + proj * delta)).norm();
            t_suggested = lower_t(i) * (1 - proj) +
                          upper_t(i) * proj;
        }
        if (d_suggested < closest)
        {
            closest = d_suggested;
            out = t_suggested;
        }
    }
    assert(!std::isinf(out));
}

Eigen::Vector3f BezierClosestPointOracle::at(float t) const
{
    return pow(1 - t, 2) * a +
           2 * (1 - t) * t * b +
           pow(t, 2) * c;
}

// Numerically solve for the gradient
void BezierClosestPointOracle::evalDerivs(
            Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                         3, 1, true> out, size_t index)
{
    const Eigen::Vector3f pt = points.col(index);
    const float epsilon = 0.001f;

    float r0, rx, ry, rz;
    evalPoint(r0, index);

    points.col(index) = pt + Eigen::Vector3f(epsilon, 0, 0);
    evalPoint(rx, index);

    points.col(index) = pt + Eigen::Vector3f(0, epsilon, 0);
    evalPoint(ry, index);

    points.col(index) = pt + Eigen::Vector3f(0, 0, epsilon);
    evalPoint(rz, index);

    out(0) = (rx - r0) / epsilon;
    out(1) = (ry - r0) / epsilon;
    out(2) = (rz - r0) / epsilon;

    // Restore point to its previous value
    points.col(index) = pt;
}

void BezierClosestPointOracle::evalFeatures(
        boost::container::small_vector<Feature, 4>& out)
{
    Eigen::Array<float, 3, Eigen::Dynamic> ds(3, 1);
    evalDerivs(ds.col(0), 0);
    out.push_back(Feature(ds.col(0)));
}
