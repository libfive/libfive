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

#pragma once

#include <algorithm>
#include "libfive/render/brep/brep.hpp"
#include "libfive/tree/oracle.hpp"

/*  The following classes and function are used to test both the oracle
 *  interface and the transformed_oracle instantiation.
 */

namespace Kernel
{

class XAsOracle : public Oracle
{
    Interval::I getRange(Region<2> region) const override
    {
        return { region.lower.x(), region.upper.x() };
    }

    Interval::I getRange(Region<3> region) const override
    {
        return { region.lower.x(), region.upper.x() };
    }

    GradientsWithEpsilons getGradients(Eigen::Vector3f point) const override
    {
        return { { {1.f, 0.f, 0.f } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point) const override
    {
        return point.x();
    }
};

class YAsOracle : public Oracle
{
    Interval::I getRange(Region<2> region) const override
    {
        return { region.lower.y(), region.upper.y() };
    }

    Interval::I getRange(Region<3> region) const override
    {
        return { region.lower.y(), region.upper.y() };
    }

    GradientsWithEpsilons getGradients(Eigen::Vector3f point) const override
    {
        return { { { 0.f, 1.f, 0.f } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point) const override
    {
        return point.y();
    }
};

class ZAsOracle : public Oracle
{
    Interval::I getRange(Region<2> region) const override
    {
        return { region.perp(0), region.perp(0) };
    }

    Interval::I getRange(Region<3> region) const override
    {
        return { region.lower.z(), region.upper.z() };
    }

    GradientsWithEpsilons getGradients(Eigen::Vector3f point) const override
    {
        return { { { 0.f, 0.f, 1.f } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point) const override
    {
        return point.z();
    }
};

Tree convertToOracleAxes(Tree t)
{
    return t.remap(Tree(std::make_unique<const XAsOracle>()), 
        Tree(std::make_unique<const YAsOracle>()), 
        Tree(std::make_unique<const ZAsOracle>()));
}

template <unsigned N>
void requireEquality(const BRep<N>& first, const BRep<N>& second)
{
    auto vertsEqual = [](
        const Eigen::Matrix<float, N, 1>& first, 
        const Eigen::Matrix<float, N, 1>& second)
    {
        for (auto i = 0; i < N; ++i)
        {
            if (first(i) != second(i))
            {
                return false;
            }
        }
        return true;
    };
    auto branesEqual = [](
        const Eigen::Matrix<uint32_t, N, 1>& first,
        const Eigen::Matrix<uint32_t, N, 1>& second)
    {
        for (auto i = 0; i < N; ++i)
        {
            if (first(i) != second(i))
            {
                return false;
            }
        }
        return true;
    };
    REQUIRE(first.verts.size() == second.verts.size());
    for (auto i = 0; i < first.verts.size(); ++i) {
        CAPTURE(i);
        REQUIRE(vertsEqual(first.verts[i], second.verts[i]));
    }
    CAPTURE(first.branes.size());
    CAPTURE(second.branes.size());
    REQUIRE(first.branes.size() == second.branes.size());
    for (auto i = 0; i < first.branes.size(); ++i) {
        CAPTURE(i);
        REQUIRE(branesEqual(first.branes[i], second.branes[i]));
    }
}

} //namespace Kernel