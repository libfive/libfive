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
#include <numeric>

#include "libfive/solve/solver.hpp"
#include "libfive/tree/tree.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/eval/eval_jacobian.hpp"

namespace Kernel {

namespace Solver
{

static std::pair<float, Solution> findRoot(
        JacobianEvaluator& e, Tape::Handle tape,
        const Eigen::Vector3f pos, Solution vars, unsigned gas)
{
    const float EPSILON = 1e-6f;

    // Create a static map for all of our derivatives
    std::map<Tree::Id, float> ds;
    for (auto& v : vars)
    {
        ds.insert({v.first, 0});
    }

    float r = e.eval(pos, tape);
    bool converged = false;
    while (!converged && fabs(r) >= EPSILON && --gas)
    {
        // Evaluate and update our local gradient
        for (auto& d : e.gradient(pos, tape))
        {
            auto v = ds.find(d.first);
            if (v != ds.end())
            {
                v->second = d.second;
            }
        }

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
                [](float d, const decltype(ds)::value_type& itr) {
                    return d + pow(itr.second, 2); });

        for (float step = r / slope; true; step /= 2)
        {
            for (auto& v : vars)
            {
                e.setVar(v.first, v.second - step * ds.at(v.first));
            }

            // Get new residual
            const auto r_ = e.eval(pos, tape);

            // Find change in residuals
            const auto diff = r - r_;

            // If we've satisfied the Armijo–Goldstein condition, then break
            // (along with a bunch of other conditions to detect infinite loops)
            if (diff / step >= slope * 0.5 || fabs(diff) < EPSILON ||
                slope < EPSILON || r_ < EPSILON)
            {
                // If residuals are converging, then exit outer loop too
                converged = fabs(diff) < EPSILON;
                r = r_;

                // Store new variable values in our map
                for (auto& v : vars)
                {
                    v.second -= step * ds.at(v.first);
                }
                break;
            }
        }
    }
    return {r, vars};
}

////////////////////////////////////////////////////////////////////////////////

std::pair<float, Solution> findRoot(
        const Tree& t, const std::map<Tree::Id, float>& vars,
        const Eigen::Vector3f pos, const Mask& mask, unsigned gas)
{
    auto deck = std::make_shared<Deck>(t);
    JacobianEvaluator e(deck, vars);
    return findRoot(e, deck->tape, vars, pos, mask, gas);
}

std::pair<float, Solution> findRoot(
        JacobianEvaluator& e, Tape::Handle tape,
        std::map<Tree::Id, float> vars, const Eigen::Vector3f pos,
        const Mask& mask, unsigned gas)
{
    // Load initial variable values here
    for (auto& v : vars)
    {
        e.setVar(v.first, v.second);
    }

    // Ignore masked variables
    for (const auto& v : mask)
    {
        vars.erase(v);
    }

    return findRoot(e, tape, pos, vars, gas);
}

} // namespace Solver

}   // namespace Kernel
