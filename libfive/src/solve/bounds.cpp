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
#include <cmath>

#include "libfive/solve/bounds.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/oracle.hpp"

namespace Kernel {

Region<3> findBounds(const Tree& t)
{
    std::map<Tree::Id, float> vars;
    return findBounds(t, vars);
}

Region<3> findBounds(const Tree& t, const std::map<Tree::Id, float>& vars)
{
    IntervalEvaluator e(std::make_shared<Tape>(t), vars);
    return findBounds(&e);
}

Region<3> findBounds(IntervalEvaluator* eval)
{
    const auto inf = std::numeric_limits<double>::infinity();
    Region<3> out({-inf, -inf, -inf}, {inf, inf, inf});

    // Helper function to load and evaluate an interval
    auto testRegion = [=](const Region<3>& r){
        auto lower = r.lower.cast<float>();
        auto upper = r.upper.cast<float>();
        return eval->eval(lower, upper).lower(); };

    // Helper function to check a particular [axis + sign + value].
    //
    // Semi-infinite half-spaces are broken into four semi-infinite
    // eighth-spaces.  This correctly handles cases like rotation,
    // where otherwise things go infinite where they don't need to.
    //
    // Non-infinite spaces are split into a bunch of sub-intervals
    // to do a better job of getting the bounds.
    auto check = [&](unsigned axis, unsigned s, double d, unsigned n){
        const int q = Axis::toIndex(Axis::Q(Axis::toAxis(axis)));
        const int r = Axis::toIndex(Axis::R(Axis::toAxis(axis)));
        float o = std::numeric_limits<float>::infinity();

        if (out.lower.isInf().any() || out.upper.isInf().any())
        {
            for (unsigned i=0; i < 4; ++i)
            {
                Region<3> target = out;
                target[s](axis) = d;
                target[(i & 1) == 0](q) = 0;
                target[(i & 2) == 0](r) = 0;
                o = fmin(o, testRegion(target));
            }
        }
        else
        {
            for (unsigned i=0; i < n; ++i)
            {
                Region<3> target = out;
                target[s](axis) = d;
                target.lower(q) = (out.lower(q) * (n - i) / float(n)) +
                                  (out.upper(q) * i / float(n));
                target.upper(q) = (out.lower(q) * (n - i - 1) / float(n)) +
                                  (out.upper(q) * (i + 1) / float(n));

                for (unsigned j=0; j < n; ++j)
                {
                    target.lower(r) = (out.lower(r) * (n - j) / float(n)) +
                                      (out.upper(r) * j / float(n));
                    target.upper(r) = (out.lower(r) * (n - j - 1) / float(n)) +
                                      (out.upper(r) * (j + 1) / float(n));
                    o = fmin(o, testRegion(target));
                }
            }
        }
        return o;
    };

    for (unsigned iter=0; iter < 8; ++iter)
    {
        Region<3> next = out;

        // This value is chosen experimentally as a subdivision scaling
        // that does a reasonable job of converging on bounds without
        // being too expensive.
        const int n = iter * 2;

        // Iterate over axes
        for (unsigned axis=0; axis < 3; ++axis)
        {
            // Iterate over positive vs negative
            for (unsigned s=0; s <= 1; ++s)
            {
                double sign = s ? -1 : 1;
                double pos = -sign;

                // First, walk back on the given axis until you find the shape
                double step = sign;
                do
                {
                    pos -= sign;
                    step *= 2;
                }
                while (check(axis, s, pos, n) > 0 && !std::isinf(step));

                // Then walk forward until you are outside of the shape
                step = sign;
                do
                {
                    pos += step;
                    step *= 2;
                }
                while (check(axis, s, pos, n) <= 0 && !std::isinf(step));

                // If we can't get a bound on this axis, keep going
                if (std::isinf(step))
                {
                    continue;
                }

                double outside = pos;
                double inside = pos - step / 2;
                double frac = 0.5;
                step = 0.25;
                for (unsigned i=0; i < 16; ++i)
                {
                    pos = outside * frac + inside * (1 - frac);
                    if (check(axis, s, pos, n) <= 0)
                    {
                        frac += step;
                    }
                    else
                    {
                        frac -= step;
                    }
                    step /= 2;
                }
                next[!s](axis) = pos;
            }
        }
        out = next;
    }
    return out;
}

}   // namespace Kernel
