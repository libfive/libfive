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
        lower_t(i) = i / n;
        upper_t(i) = (i + 1) / n;
        lower.row(i) = at(lower_t(i));
        upper.row(i) = at(lower_t(i));
    }
}

void BezierClosestPointOracle::evalInterval(Interval::I& out)
{
    Interval::I X(lower(0), upper(0));
    Interval::I Y(lower(1), upper(1));
    Interval::I Z(lower(2), upper(2));

    for (unsigned i=0; i < lower.rows(); ++i)
    {
        const Eigen::Vector3f u = lower.row(i);
        const Eigen::Vector3f v = upper.row(i);
        const Eigen::Vector3f delta = u - v;
        const float len = delta.norm();

        // This is the projected distance of the interval
        // onto a particular line segment.
        Interval::I proj = (X - u.x()) * delta.x() / len +
                           (Y - u.y()) * delta.y() / len +
                           (Z - u.z()) * delta.z() / len;
    }
}

void BezierClosestPointOracle::evalPoint(float& out, size_t index)
{
    Eigen::Vector3f pt = points.col(index);

    float closest = std::numeric_limits<float>::infinity();
    for (unsigned i=0; i < lower.rows(); ++i)
    {
        const Eigen::Vector3f u = lower.row(i);
        const Eigen::Vector3f v = upper.row(i);
        const Eigen::Vector3f delta = u - v;
        const float len = delta.norm();

        // This is the projected distance of the interval
        // onto a particular line segment.
        float proj = (pt.x() - u.x()) * delta.x() / len +
                     (pt.y() - u.y()) * delta.y() / len +
                     (pt.z() - u.z()) * delta.z() / len;

        float d_suggested = std::numeric_limits<float>::infinity();
        float t_suggested = std::nanf("");

        // Convert proj to a 0-to-1 value that represents a sitance
        // along this particular line segment.
        proj /= len;
        if (i == 0 && proj <= 0)
        {
            d_suggested = (pt - u).norm();
            t_suggested = 1;
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
}

Eigen::Vector3f BezierClosestPointOracle::at(float t) const
{
    return pow(1 - t, 2) * a +
           2 * (1 - t) * t * b +
           pow(t, 2) * c;
}
