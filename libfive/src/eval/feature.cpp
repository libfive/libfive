/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/feature.hpp"

namespace libfive {

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

bool Feature::push(const Feature& other)
{
    for (auto& e : other.epsilons) {
        if (!push(e)) {
            return false;
        }
    }
    return true;
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

    // Construct a potential set of epsilons, which includes the new point
    auto es = epsilons;
    es.push_back(e);

    // Each epsilon implies a plane.  The intersection of all of these
    // which is a polyhedral convex cone.  We want to check whether the
    // cone is empty or not.  To do so, we build a bounded polytope by
    // intersecting the cone with a small cube, then check every possible
    // vertex to see if it's valid.  If the only valid vertex is at the
    // origin, then the cone is empty.
    //
    // This is not particularly inefficient, and could be replaced with
    // a linear programming system, but N should be relatively small.

    // These are the planes of the polytope, in the form n * x = d
    using boost::container::small_vector;
    small_vector<std::pair<Eigen::Vector3f, float>, 16> planes;
    for (const auto& e : es) {
        planes.push_back(std::make_pair(e, 0.0));
    }
    // Add the six planes defining the cube
    planes.push_back(std::make_pair(Eigen::Vector3f(1, 0, 0), 1));
    planes.push_back(std::make_pair(Eigen::Vector3f(-1, 0, 0), 1));
    planes.push_back(std::make_pair(Eigen::Vector3f(0, 1, 0), 1));
    planes.push_back(std::make_pair(Eigen::Vector3f(0, -1, 0), 1));
    planes.push_back(std::make_pair(Eigen::Vector3f(0, 0, 1), 1));
    planes.push_back(std::make_pair(Eigen::Vector3f(0, 0, -1), 1));

    // We build the polytope corners with an expensive loop
    small_vector<Eigen::Vector3f, 32> corners;
    for (unsigned i=0; i < planes.size(); ++i) {
        for (unsigned j=i; j < planes.size(); ++j) {
            for (unsigned k=j; k < planes.size(); ++k) {
                const auto a = planes[i];
                const auto b = planes[j];
                const auto c = planes[k];

                // Skip corners that are entirely from cone planes, which
                // have a degenerate solution at the origin
                if (a.second == 0.0 && b.second == 0.0 && c.second == 0.0) {
                    continue;
                }

                Eigen::Matrix3f M;
                M << a.first, b.first, c.first;

                Eigen::Vector3f d;
                d << a.second, b.second, c.second;

                // Skip matrices with a bad determinant, which can happen
                // if the two cone normals are nearly identical
                const auto det = M.determinant();
                if (fabs(det) < 1e-6) {
                    continue;
                }
                const auto vert = M.inverse() * d;
                bool duplicate = false;
                for (const auto& c : corners) {
                    if (c == vert) {
                        duplicate = true;
                        break;
                    }
                }
                if (!duplicate) {
                    corners.push_back(vert);
                }
            }
        }
    }

    // Now, we check every epsilon against every possible corner, returning
    // true when we find a corner that's compatible with each one.
    for (const auto& c : corners) {
        bool okay = true;
        for (const auto& e : es) {
            if (c.dot(e) <= 0) {
                okay = false;
                break;
            }
        }
        if (okay) {
            return true;
        }
    }
    // Extremely hacky extra loop: there are cases where there exists a valid
    // point, but it is in between a set of corners.  For example, consider
    // the three epsilons
    //     [-1,  0, 0]
    //     [ 0, -1, 0]
    //     [-1,  1, 0]
    //
    //  These form a Z-extrusion polytope which looks like this:
    //
    //   --------O    (O marks the origin)
    //   |      /
    //   X    /
    //   |  /
    //   |/
    //  but the corners of this polytope all have one vector for which the
    //  dot product is exactly zero.  We want to select the point marked with
    //  an X, but it's not on any corner.
    for (const auto& c : corners) {
        for (const auto& d : corners) {
            const Eigen::Vector3f p = (c + d) / 2.0f;
            bool okay = true;
            for (const auto& e : es) {
                if (p.dot(e) <= 0) {
                    okay = false;
                    break;
                }
            }
            if (okay) {
                return true;
            }
        }
    }
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

}   // namespace libfive
