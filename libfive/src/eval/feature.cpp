/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/feature.hpp"

namespace Kernel {

Feature::Feature(const Eigen::Vector3f& d)
    : deriv(d)
{
    // Nothing to do here
}

Feature::Feature(const Eigen::Vector3f& d, const Feature& a)
    : deriv(d), epsilons(a.epsilons)
{
    // Nothing to do here
}

Feature::Feature(const Eigen::Vector3f& d, const Feature& a, const Feature& b)
    : deriv(d), epsilons(a.epsilons)
{
    // Merge and deduplicate epsilons
    // This is O(N^2), but it should be fine for small N
    for (auto& e : b.epsilons)
    {
        bool found = false;
        for (auto& e_ : epsilons)
        {
            if (e == e_)
            {
                found = true;
                break;
            }
        }

        if (!found)
        {
            epsilons.push_back(e);
        }
    }
}

Feature::Feature(const Feature& a, const Eigen::Matrix3f& transform)
    : deriv(transform * a.deriv), epsilons(a.epsilons)
{
    for (auto& e : epsilons)
    {
        e = transform * e;
    }
}

bool Feature::push(const Eigen::Vector3f& e_)
{
    const auto norm = e_.norm();
    if (norm == 0)
    {
        return false;
    }

    Eigen::Vector3f e = e_ / norm;
    bool dup = false;
    if (check(e, &dup))
    {
        if (!dup)
        {
            epsilons.push_back(e);
        }
        return true;
    }
    else
    {
        return false;
    }
}

bool Feature::check(const Feature& other) const
{
    auto temp = *this;
    return std::all_of(other.epsilons.begin(), other.epsilons.end(),
            [&](const Eigen::Vector3f& e){ return temp.push(e); });
}

bool Feature::check(const Eigen::Vector3f& e, bool* duplicate) const
{
    // Return early if the epsilon is already in the list
    for (const auto& i : epsilons)
    {
        if (e.dot(i) > 1 - 1e-8)
        {
            if (duplicate)
            {
                *duplicate = true;
            }
            return true;
        }
    }

    if (epsilons.size() == 0)
    {
        return true;
    }
    if (epsilons.size() == 1)
    {
        return (e.dot(epsilons.front()) != -1);
    }

    {   // Check for planarity (2D special case)
        auto itr = epsilons.begin();
        const auto cross = itr->cross(e);
        const auto cross_ = cross.normalized();

        const auto angle = asin(cross.norm());
        auto angle_min = fmin(0.0, angle);
        auto angle_max = fmax(0.0, angle);

        while (++itr != epsilons.end())
        {
            auto c = itr->cross(e);
            auto c_ = c.normalized();

            // Early exit from the loop if values are non-planar
            if (fabs(c_.dot(cross_)) != 1)
            {
                break;
            }

            const auto angle = asin(c.norm());
            angle_min = fmin(angle, angle_min);
            angle_max = fmax(angle, angle_max);
        }

        if (itr == epsilons.end())
        {
            return !(angle_max - angle_min > M_PI);
        }
    }

    {   // Otherwise, we construct every possible plane and check against
        // every remaining point to make sure they work
        auto es = epsilons;
        es.push_back(e);

        // Yes, this is an O(n^3) loop
        // It's far from optimal, but will suffice unless people start making
        // deliberately pathological models.
        for (auto a=es.begin(); a != es.end(); ++a)
        {
            for (auto b=es.begin(); b != es.end(); ++b)
            {
                if (a == b || a->dot(*b) == -1)
                {
                    continue;
                }
                const auto norm = a->cross(*b);
                int sign = 0;
                bool passed = true;
                for (auto c=es.begin(); passed && c != es.end(); ++c)
                {
                    if (a == c || b == c)
                    {
                        continue;
                    }
                    auto d = norm.dot(*c);
                    if (d < 0)
                    {
                        passed &= (sign <= 0);
                        sign = -1;
                    }
                    else if (d > 0)
                    {
                        passed &= (sign >= 0);
                        sign = 1;
                    }
                    else
                    {
                        passed = false;
                    }
                }
                if (passed)
                {
                    return true;
                }
            }
        }
    }

    // If we've made it through the whole loop with no matches, then
    return false;
}

bool Feature::operator<(const Feature& other) const
{
    auto EigenCompare =
        [](const Eigen::Vector3f& a, const Eigen::Vector3f& b)
    {
        for (auto i = 0; i < 3; ++i)
        {
            if (a(i) < b(i))
            {
                return -1;
            }
            else if (a(i) > b(i))
            {
                return 1;
            }
        }
        return 0;
      };
    size_t i = 0;
    for (; i < epsilons.size() && i < other.epsilons.size(); ++i)
    {
        auto comp = EigenCompare(epsilons[i], other.epsilons[i]);
        if (comp != 0)
        {
            return comp < 0;
        }
    }
    if (i < epsilons.size())
    {
        return false;
    }
    else if (i < other.epsilons.size())
    {
        return true;
    }
    else
    {
        return EigenCompare(deriv, other.deriv) < 0;
    }
}

}   // namespace Kernel
