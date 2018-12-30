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

    void evalPoint(double& out, size_t index) override
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
        Eigen::Vector3d v = Eigen::Vector3d::Zero();
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
            max(-(X + 1.5), X - 1.5),
            max(-(Y + 1.5), Y - 1.5)),
            max(-(Z + 1.5), Z - 1.5));
    }

    void evalPoint(double& out, size_t index) override
    {
        const auto x = points(0, index);
        const auto y = points(1, index);
        const auto z = points(2, index);

        out = fmax(fmax(
            fmax(-(x + 1.5), x - 1.5),
            fmax(-(y + 1.5), y - 1.5)),
            fmax(-(z + 1.5), z - 1.5));
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
        auto push = [&](Eigen::Vector3d d){ out.push_back(Feature(d)); };

        Eigen::Vector3d p = points.col(0);
        if (fabs(p.x()) >= fmax(fabs(p.y()), fabs(p.z())))
        {
            if (p.x() >= 0.0)
            {
                push({ 1.0, 0.0, 0.0 });
            }
            if (p.x() <= 0.0)
            {
                push({ -1.0, 0.0, 0.0 });
            }
        }
        if (fabs(p.y()) >= fmax(fabs(p.x()), fabs(p.z())))
        {
            if (p.y() >= 0.0)
            {
                push({ 0.0, 1.0, 0.0 });
            }
            if (p.y() <= 0.0)
            {
                push({ 0.0, -1.0, 0.0 });
            }
        }
        if (fabs(p.z()) >= fmax(fabs(p.y()), fabs(p.x())))
        {
            if (p.z() >= 0.0)
            {
                push({ 0.0, 0.0, 1.0 });
            }
            if (p.z() <= 0.0)
            {
                push({ 0.0, 0.0, -1.0 });
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
