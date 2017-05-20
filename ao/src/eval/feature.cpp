#include <glm/glm.hpp>
#include "kernel/eval/feature.hpp"

namespace Kernel {

bool Feature::isCompatible(glm::vec3 e) const
{
    {   // Normalize based on vector length
        const auto norm = glm::length(e);
        if (norm == 0)
        {
            return false;
        }
        e /= norm;
    }

    if (epsilons.size() == 0)
    {
        return true;
    }
    else if (epsilons.size() == 1)
    {
        return glm::dot(e, epsilons.front()) != -1;
    }

    // Return early if the epsilon is already in the list
    for (const auto& i : epsilons)
    {
        if (e == i)
        {
            return true;
        }
    }

    // Special case for 2D (planar) sets of points
    switch (checkPlanar(e))
    {
        case PLANAR_FAIL: return false;
        case PLANAR_SUCCESS: return true;
        case NOT_PLANAR: break;
    }

    // Otherwise, we construct every possible plane and check against
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
            if (a == b || glm::dot(*a, *b) == -1)
            {
                continue;
            }
            const auto norm = glm::cross(*a, *b);
            int sign = 0;
            bool passed = true;
            for (auto c=es.begin(); passed && c != es.end(); ++c)
            {
                if (a == c || b == c)
                {
                    continue;
                }
                auto d = glm::dot(norm, *c);
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
    return false;
}

void Feature::push_raw(Choice c, glm::vec3 v)
{
    v /= glm::length(v);

    epsilons.push_back(v);
    choices.push_back(c);
    _epsilons[c.id] = v;
}

bool Feature::push(glm::vec3 e, Choice choice)
{
    if (isCompatible(e))
    {
        choices.push_front(choice);
        _epsilons[choice.id] = e;

        // Store the epsilon if it isn't already present
        e /= glm::length(e);
        for (auto i : epsilons)
        {
            if (e == i)
            {
                return true;
            }
        }
        epsilons.push_back(e);
        return true;
    }
    else
    {
        return false;
    }
}

bool operator<(const Feature::Choice& a, const Feature::Choice& b)
{
    if (a.id != b.id)
    {
        return a.id < b.id;
    }
    return a.choice < b.choice;
}

Feature::PlanarResult Feature::checkPlanar(glm::vec3 v) const
{
    if (epsilons.size() < 2)
    {
        return NOT_PLANAR;
    }

    v /= glm::length(v);

    auto itr = epsilons.begin();
    const auto cross = glm::cross(*itr, v);
    const auto cross_ = cross / glm::length(cross);

    const auto angle = asin(glm::length(cross));
    auto angle_min = std::min(0.0, angle);
    auto angle_max = std::max(0.0, angle);

    while (++itr != epsilons.end())
    {
        auto c = glm::cross(*itr, v);
        auto c_ = c / glm::length(c);
        if (std::abs(glm::dot(c_, cross_)) != 1)
        {
            return NOT_PLANAR;
        }

        const auto angle = asin(glm::length(c));
        angle_min = std::min(angle, angle_min);
        angle_max = std::max(angle, angle_max);
    }

    return (angle_max - angle_min > M_PI) ? PLANAR_FAIL : PLANAR_SUCCESS;
}

}   // namespace Kernel
