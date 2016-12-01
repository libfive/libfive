/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <numeric>

#include "ao/kernel/solve/solver.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

namespace Solver
{

std::pair<float, Solution> findRoot(const Tree& t, const glm::vec3 v)
{
    const float EPSILON = 1e-6;

    // Find our initial variables and residual
    auto e = Evaluator(t);
    auto vars = e.varValues();
    float r = e.eval(v.x, v.y, v.z);

    bool converged = false;

    while (!converged && fabs(r) >= EPSILON)
    {
        // Vars should be set from the most recent evaluation
        const auto ds = e.gradient(v.x, v.y, v.z);

        // Break if all of our gradients are nearly zero
        if (std::all_of(ds.begin(), ds.end(),
            [&EPSILON](decltype(ds)::value_type itr) {
                return fabs(itr.second) < EPSILON;
            }))
        {
            break;
        }

        // Solve for step size using a backtracking line search
        const float slope = std::accumulate(ds.begin(), ds.end(), 0.0f,
                [](float v, const decltype(ds)::value_type& itr) {
                    return v + pow(itr.second, 2); });

        for (float step = r / slope; true; step /= 2)
        {
            for (const auto& v : vars)
            {
                e.setVar(v.first, v.second - step * ds.at(v.first));
            }

            // Get new residual
            const auto r_ = e.eval(v.x, v.y, v.z);

            // Find change in residuals
            const auto diff = r - r_;

            // If we've satisfied the Armijo–Goldstein condition, then break
            if (diff / step >= slope * 0.5)
            {
                // If residuals are converging, then exit outer loop too
                converged = fabs(diff) < EPSILON;
                r = r_;
                break;
            }
        }

        // Extract new variable table
        vars = e.varValues();
    }

    return {r, vars};
}

} // namespace Solver
