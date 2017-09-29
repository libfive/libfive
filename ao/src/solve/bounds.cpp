#include "ao/solve/bounds.hpp"

namespace Kernel {

Region<3> findBounds(const Tree& t)
{
    std::map<Tree::Id, float> vars;
    return findBounds(t, vars);
}

Region<3> findBounds(const Tree& t, const std::map<Tree::Id, float>& vars)
{
    Evaluator e(t, vars);
    return findBounds(&e);
}

Region<3> findBounds(Evaluator* eval)
{
    const auto inf = std::numeric_limits<double>::infinity();
    Region<3> out({-inf, -inf, -inf}, {inf, inf, inf});

    // Helper function to load and evaluate an interval
    auto check = [=](const Region<3>& r){
        eval->set(r.lower.template cast<float>(),
                  r.upper.template cast<float>());
        return eval->interval().lower(); };

    // Iterate over axes
    for (unsigned axis=0; axis < 3; ++axis)
    {
        // Iterate over positive vs negative
        for (unsigned s=0; s <= 1; ++s)
        {
            Region<3> r({-inf, -inf, -inf}, {inf, inf, inf});
            double sign = s ? -1 : 1;

            // First, walk back on the given axis until you find the shape
            r[s](axis) = 0;
            double step = sign;
            do
            {
                r[s](axis) -= sign;
                step *= 2;
            }
            while (check(r) > 0 && !std::isinf(step));

            // Then walk forward until you are outside of the shape
            step = sign;
            do
            {
                r[s](axis) += step;
                step *= 2;
            }
            while (check(r) <= 0 && !std::isinf(step));

            // If we can't get a bound on this axis, keep going
            if (std::isinf(step))
            {
                continue;
            }

            double outside = r[s](axis);
            double inside = r[s](axis) - step / 2;
            double frac = 0.5;
            step = 0.25;
            for (unsigned i=0; i < 16; ++i)
            {
                r[s](axis) = outside * frac + inside * (1 - frac);
                if (check(r) <= 0)
                {
                    frac += step;
                }
                else
                {
                    frac -= step;
                }
                step /= 2;
            }
            out[s](axis) = r[s](axis);
        }
    }
    return out;
}

}   // namespace Kernel
