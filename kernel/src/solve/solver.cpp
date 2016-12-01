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

    while (true)
    {
        // Vars should be set from the most recent evaluation
        auto ds = e.gradient(v.x, v.y, v.z);

        // Break if all of our gradients are nearly zero
        if (std::accumulate(ds.begin(), ds.end(), true,
            [&EPSILON](bool b, decltype(ds)::value_type itr) {
                return b && fabs(itr.second) < EPSILON;
                }))
        {
            break;
        }

        // Solve for step size using a backtracking line search
        // Find a termination threshold as norm(ds) / 2
        const float slope = std::accumulate(ds.begin(), ds.end(), 0,
                [](float v, decltype(ds)::value_type itr) {
                    return v + pow(itr.second, 2); });
        bool converged = false;
        for (float step=1; true; step /= 2)
        {
            for (const auto& v : vars)
            {
                e.setVar(v.first, v.second + step * ds.at(v.first));
            }

            // Get new residual
            const auto r_ = e.eval(v.x, v.y, v.z);

            // Find change in residuals
            const auto diff = r - r_;
            if (diff >= step * slope * 0.5)
            {
                // If residuals are converging, then break out of outer loop
                converged = fabs(diff) < EPSILON;
                r = r_;
                break;
            }
        }

        // Extract new variable table
        vars = e.varValues();

        // If the line search converged, then break right away
        if (converged || fabs(r) < EPSILON)
        {
            break;
        }
    }

    return {r, vars};
}

} // namespace Solver

/*
step eqn vars =
    if r < epsilon || all ((< epsilon) . abs) ds || converged
    then Nothing
    else Just next
    where (r, ds) = eval eqn vars
          (next, converged) = backtrack 1
          threshold = 0.5 * (sum $ Map.map (^2) ds)
          backtrack stepSize =
              if r - r' >= stepSize * threshold
              then (vars', abs (r - r') < epsilon)
              else backtrack (stepSize * 0.5)
              where vars' = Map.unionWith (-) vars $
                            Map.map (*stepSize) ds
                    r' = fst (eval eqn vars')
                    */

