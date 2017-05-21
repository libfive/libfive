#include <numeric>

#include "ao/solve/solver.hpp"
#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

namespace Solver
{

static std::pair<float, Solution> findRoot(
        Evaluator& e, const Eigen::Vector3f pos, const Mask& mask, unsigned gas)
{
    const float EPSILON = 1e-6;

    auto filter = [&](std::map<Tree::Id, float>& vs){
        for (const auto& v : mask)
        {
            vs.erase(v);
        }
    };
    // Find our initial variables and residual
    auto vars = e.varValues();
    filter(vars);

    float r = e.eval(pos);

    bool converged = false;

    while (!converged && fabs(r) >= EPSILON && --gas)
    {
        // Vars should be set from the most recent evaluation
        auto ds = e.gradient(pos);
        filter(ds);

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
            const auto r_ = e.eval(pos);

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
                break;
            }
        }

        // Extract new variable table
        vars = e.varValues();
        filter(vars);
    }
    return {r, vars};
}

////////////////////////////////////////////////////////////////////////////////

std::pair<float, Solution> findRoot(
        const Tree& t, const std::map<Tree::Id, float>& vars,
        const Eigen::Vector3f pos, const Mask& mask, unsigned gas)
{
    Evaluator e(t, vars);
    return findRoot(e, pos, mask, gas);
}

std::pair<float, Solution> findRoot(
        Evaluator& e, const std::map<Tree::Id, float>& vars,
        const Eigen::Vector3f pos, const Mask& mask, unsigned gas)
{
    // Load initial variable values here
    for (auto& v : vars)
    {
        e.setVar(v.first, v.second);
    }
    return findRoot(e, pos, mask, gas);
}

} // namespace Solver

}   // namespace Kernel
