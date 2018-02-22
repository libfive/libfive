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
    Interval::I getRange(Region<2> region, int threadNo) const override
    {
        return { region.lower.x(), region.upper.x() };
    }

    Interval::I getRange(Region<3> region, int threadNo) const override
    {
        return { region.lower.x(), region.upper.x() };
    }

    GradientsWithEpsilons getGradients(
        Eigen::Vector3f point, int threadNo) const override
    {
        return { { {1., 0., 0. } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point, int threadNo) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point, int threadNo) const override
    {
        return point.x();
    }
};

class YAsOracle : public Oracle
{
    Interval::I getRange(Region<2> region, int threadNo) const override
    {
        return { region.lower.y(), region.upper.y() };
    }

    Interval::I getRange(Region<3> region, int threadNo) const override
    {
        return { region.lower.y(), region.upper.y() };
    }

    GradientsWithEpsilons getGradients(
        Eigen::Vector3f point, int threadNo) const override
    {
        return { { { 0., 1., 0. } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point, int threadNo) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point, int threadNo) const override
    {
        return point.y();
    }
};

class ZAsOracle : public Oracle
{
    Interval::I getRange(Region<2> region, int threadNo) const override
    {
        return { region.perp(0), region.perp(0) };
    }

    Interval::I getRange(Region<3> region, int threadNo) const override
    {
        return { region.lower.z(), region.upper.z() };
    }

    GradientsWithEpsilons getGradients(
        Eigen::Vector3f point, int threadNo) const override
    {
        return { { { 0., 0., 1. } }, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point, int threadNo) const override
    {
        return false;
    }

    float getValue(Eigen::Vector3f point, int threadNo) const override
    {
        return point.z();
    }
};

inline Tree convertToOracleAxes(Tree t)
{
    return t.remap(Tree(std::make_unique<const XAsOracle>()), 
        Tree(std::make_unique<const YAsOracle>()), 
        Tree(std::make_unique<const ZAsOracle>()));
}

//We also want something to test cases where the gradients are not continuous.

class cubeAsOracle : public Oracle
{
    public:
    Interval::I getRange(Region<2> region, int threadNo) const override
    {
        auto minX = (region.lower.x() < 0 && region.upper.x() > 0)
            ? -1.5
            : std::min(abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto maxX = std::max(
            abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto minY = (region.lower.y() < 0 && region.upper.y() > 0)
            ? -1.5
            : std::min(abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto maxY = std::max(
            abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto z = abs(region.perp(0)) - 1.5;
        return { std::max({ minX, minY, z }),std::max({ maxX, maxY, z }) };
    }

    Interval::I getRange(Region<3> region, int threadNo) const override
    {
        auto minX = (region.lower.x() < 0 && region.upper.x() > 0)
            ? -1.5
            : std::min(abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto maxX = std::max(
            abs(region.lower.x()) - 1.5, abs(region.upper.x()) - 1.5);
        auto minY = (region.lower.y() < 0 && region.upper.y() > 0)
            ? -1.5
            : std::min(abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto maxY = std::max(
            abs(region.lower.y()) - 1.5, abs(region.upper.y()) - 1.5);
        auto minZ = (region.lower.z() < 0 && region.upper.z() > 0)
            ? -1.5
            : std::min(abs(region.lower.z()) - 1.5, abs(region.upper.z()) - 1.5);
        auto maxZ = std::max(
            abs(region.lower.z()) - 1.5, abs(region.upper.z()) - 1.5);
        return { std::max({ minX, minY, minZ }),std::max({ maxX, maxY, maxZ }) };
    }

    GradientsWithEpsilons
        getGradients(Eigen::Vector3f point, int threadNo) const override
    {
        boost::container::small_vector<Eigen::Vector3d, 1> out;
        if (abs(point.x()) >= std::max(abs(point.y()), abs(point.z())))
        {
            if (point.x() >= 0.f)
            {
                out.push_back({ 1., 0., 0. });
            }
            if (point.x() <= 0.f)
            {
                out.push_back({ -1., 0., 0. });
            }
        }
        if (abs(point.y()) >= std::max(abs(point.x()), abs(point.z())))
        {
            if (point.y() >= 0.f)
            {
                out.push_back({ 0., 1., 0. });
            }
            if (point.y() <= 0.f)
            {
                out.push_back({ 0., -1., 0. });
            }
        }
        if (abs(point.z()) >= std::max(abs(point.y()), abs(point.x())))
        {
            if (point.z() >= 0.f)
            {
                out.push_back({ 0., 0., 1. });
            }
            if (point.z() <= 0.f)
            {
                out.push_back({ 0., 0., -1. });
            }
        }
        return { out, GradientsWithEpsilons::USECLOSEST };
    }

    bool isAmbiguous(Eigen::Vector3f point, int threadNo) const override
    {
        if (abs(point.x()) == abs(point.y()))
        {
            return abs(point.x()) >= abs(point.z());
            //If true, this can return 3 or 6 gradients, depending on whether
            //it's 0, but is ambiguous either way.
        }
        else
        {
            return std::max(abs(point.x()), abs(point.y())) == abs(point.z());
        }
    }

    float getValue(Eigen::Vector3f point, int threadNo) const override
    {
        return std::max({ abs(point.x()), abs(point.y()), abs(point.z()) }) - 1.5f;
    }
};

template <unsigned N>
inline void requireEquality(const BRep<N>& first, const BRep<N>& second)
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
        CAPTURE(first.verts[i]);
        CAPTURE(second.verts[i]);
        REQUIRE(vertsEqual(first.verts[i], second.verts[i]));
    }
    CAPTURE(first.branes.size());
    CAPTURE(second.branes.size());
    REQUIRE(first.branes.size() == second.branes.size());
    for (auto i = 0; i < first.branes.size(); ++i) {
        CAPTURE(i);
        CAPTURE(first.branes[i]);
        CAPTURE(second.branes[i]);
        REQUIRE(branesEqual(first.branes[i], second.branes[i]));
    }
}

} //namespace Kernel