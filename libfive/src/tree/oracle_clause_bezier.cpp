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

#include "libfive/tree/oracle_clause_bezier.hpp"
#include "libfive/eval/oracle_bezier.hpp"

using namespace Kernel;

BezierClosestPointOracleClause::BezierClosestPointOracleClause(
        const Eigen::Vector3f a,
        const Eigen::Vector3f b,
        const Eigen::Vector3f c)
    : a(a), b(b), c(c)
{
    // Nothing to do here
}

std::unique_ptr<Oracle> BezierClosestPointOracleClause::getOracle() const
{
    auto o = new BezierClosestPointOracle(a, b, c);
    return std::unique_ptr<Oracle>(o);
}
