#include "ao/eval/feature.hpp"

namespace Kernel {

bool Feature::isCompatible(Eigen::Vector3d e) const
{
    const auto norm = e.norm();
    return (norm == 0) ? false : isCompatibleNorm(e / norm);
}

bool Feature::isCompatibleNorm(Eigen::Vector3d e) const
{
    if (epsilons.size() == 0)
    {
        return true;
    }
    else if (epsilons.size() == 1)
    {
        return e.dot(epsilons.front()) != -1;
    }

    // Return early if the epsilon is already in the list
    for (const auto& i : epsilons)
    {
        if (e.dot(i) > 1 - 1e-8)
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
    return false;
}

void Feature::pushRaw(Choice c, Eigen::Vector3d v)
{
    v.normalize();

    epsilons.push_back(v);
    choices.push_back(c);
    _epsilons[c.id] = v;
}

void Feature::pushChoiceRaw(Choice c)
{
    choices.push_back(c);
}

void Feature::pushChoice(Choice c)
{
    choices.push_front(c);
}

bool Feature::push(Eigen::Vector3d e, Choice choice)
{
    const auto norm = e.norm();
    return (norm == 0) ? false : pushNorm(e / norm, choice);
}

bool Feature::pushNorm(Eigen::Vector3d e, Choice choice)
{
    if (isCompatibleNorm(e))
    {
        choices.push_front(choice);
        _epsilons[choice.id] = e;

        // Store the epsilon if it isn't already present
        for (auto& i : epsilons)
        {
            if (e.dot(i) > 1 - 1e-8)
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

Feature::PlanarResult Feature::checkPlanar(Eigen::Vector3d v) const
{
    if (epsilons.size() < 2)
    {
        return NOT_PLANAR;
    }

    v.normalize();

    auto itr = epsilons.begin();
    const auto cross = itr->cross(v);
    const auto cross_ = cross.normalized();

    const auto angle = asin(cross.norm());
    auto angle_min = std::min(0.0, angle);
    auto angle_max = std::max(0.0, angle);

    while (++itr != epsilons.end())
    {
        auto c = itr->cross(v);
        auto c_ = c.normalized();
        if (std::abs(c_.dot(cross_)) != 1)
        {
            return NOT_PLANAR;
        }

        const auto angle = asin(c.norm());
        angle_min = std::min(angle, angle_min);
        angle_max = std::max(angle, angle_max);
    }

    return (angle_max - angle_min > M_PI) ? PLANAR_FAIL : PLANAR_SUCCESS;
}

}   // namespace Kernel
