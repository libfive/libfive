/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/oracle/oracle_storage.hpp"
#include "libfive/oracle/oracle_clause.hpp"
#include "libfive/tree/tree.hpp"

namespace Kernel {

// This oracle wraps the X, Y, or Z axis
template <int A>
class AxisOracle : public OracleStorage<>
{
    void evalInterval(Interval::I& out) override
    {
        out = {lower(A), upper(A)};
    }

    void evalPoint(float& out, size_t index) override
    {
        out = points(A, index);
    }

    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                         1, Eigen::Dynamic> /* out */) override
    {
        // Nothing to do here
    }

    void evalFeatures(
            boost::container::small_vector<Feature, 4>& out) override
    {
        Eigen::Vector3f v = Eigen::Vector3f::Zero();
        v(A) = 1;
        out.push_back(Feature(v));
    }
};

// Oracle clause for a particular axis, constructing AxisOracle objects
template <int A>
class AxisOracleClause : public OracleClause
{
    std::unique_ptr<Oracle> getOracle() const override
    {
        return std::unique_ptr<Oracle>(new AxisOracle<A>());
    }

    std::string name() const override
    {
        return "AxisOracle" + std::to_string(A);
    }
};

// Replaces X, Y, and Z with oracles that pretend to be them
inline Tree convertToOracleAxes(Tree t)
{
    return t.remap(
        Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<0>)),
        Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<1>)),
        Tree(std::unique_ptr<OracleClause>(new AxisOracleClause<2>)));
}

////////////////////////////////////////////////////////////////////////////////

class CubeOracle : public OracleStorage<>
{
    void evalInterval(Interval::I& out) override
    {
        using namespace boost::numeric; // for max

        Interval::I X(lower.x(), upper.x());
        Interval::I Y(lower.y(), upper.y());
        Interval::I Z(lower.z(), upper.z());

        out = max(max(
            max(-(X + 1.5f), X - 1.5f),
            max(-(Y + 1.5f), Y - 1.5f)),
            max(-(Z + 1.5f), Z - 1.5f));
    }

    void evalPoint(float& out, size_t index) override
    {
        float x = points(0, index);
        float y = points(1, index);
        float z = points(2, index);

        out = fmax(fmax(
            fmax(-(x + 1.5f), x - 1.5f),
            fmax(-(y + 1.5f), y - 1.5f)),
            fmax(-(z + 1.5f), z - 1.5f));
    }

    void checkAmbiguous(
            Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
            1, Eigen::Dynamic> out) override
    {
        out = out ||
            (points.leftCols(out.cols()).row(0).cwiseAbs() ==
             points.leftCols(out.cols()).row(1)).cwiseAbs() ||
            (points.leftCols(out.cols()).row(0).cwiseAbs() ==
             points.leftCols(out.cols()).row(2)).cwiseAbs() ||
            (points.leftCols(out.cols()).row(1).cwiseAbs() ==
             points.leftCols(out.cols()).row(2).cwiseAbs());
    }

    void evalFeatures(
            boost::container::small_vector<Feature, 4>& out) override
    {
        // We don't properly push epsilons, but that's okay for this
        // basic test (where we don't encounter other features).
        auto push = [&](Eigen::Vector3f d){ out.push_back(Feature(d)); };

        Eigen::Vector3f p = points.col(0);
        if (fabs(p.x()) >= fmax(fabs(p.y()), fabs(p.z())))
        {
            if (p.x() >= 0.f)
            {
                push({ 1.f, 0.f, 0.f });
            }
            if (p.x() <= 0.f)
            {
                push({ -1.f, 0.f, 0.f });
            }
        }
        if (fabs(p.y()) >= fmax(fabs(p.x()), fabs(p.z())))
        {
            if (p.y() >= 0.f)
            {
                push({ 0.f, 1.f, 0.f });
            }
            if (p.y() <= 0.f)
            {
                push({ 0.f, -1.f, 0.f });
            }
        }
        if (fabs(p.z()) >= fmax(fabs(p.y()), fabs(p.x())))
        {
            if (p.z() >= 0.f)
            {
                push({ 0.f, 0.f, 1.f });
            }
            if (p.z() <= 0.f)
            {
                push({ 0.f, 0.f, -1.f });
            }
        }
    }
};

class CubeOracleClause : public OracleClause
{
    std::unique_ptr<Oracle> getOracle() const override
    {
        return std::unique_ptr<Oracle>(new CubeOracle());
    }

    std::string name() const override
    {
        return "CubeOracle";
    }
};


}   // namespace Kernel
