/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <boost/container/small_vector.hpp>
#include <Eigen/Eigen>

namespace libfive
{

/*  A Feature contains a axis-wise partial derivatives (in d), and a set
 *  of epsilons that are needed in order to select this feature.
 *
 *  For example, consider max(x, y) at x == y == 0, which has two features:
 *    d = [1,0,0], epsilons=[[ 0.7, -0.7, 0]]
 *    d = [0,1,0], epsilons=[[-0.7,  0.7, 0]]
 *
 *  epsilons should be normalized to a length of 1.
 */
class Feature {
public:
    Feature(const Eigen::Vector3f& d);

    /*
     *  Constructs a feature with a single parent
     */
    Feature(const Eigen::Vector3f& d, const Feature& a);

    /*
     *  Construct a feature with two parents, merging and deduplicating
     *  their lists of epsilons.  Note that the two parent features must
     *  already be checked as compatible; no checking is performed here.
     */
    Feature(const Eigen::Vector3f& d, const Feature& a, const Feature& b);

    /*
     *  Transforms the derivative and all epsilons
     *  according to the given transformation.
     */
    Feature(const Feature& a, const Eigen::Matrix3f& transform);

    /*
     *  If incompatible, does nothing and returns false
     *  Otherwise, pushes to the front of the choice list and returns true
     */
    bool push(const Eigen::Vector3f& e);
    bool push(const Feature& other);

    /*
     *  Checks to see whether the given epsilon is compatible with
     *  the existing epsilon vector.
     */
    bool check(const Eigen::Vector3f& e, bool* dup=nullptr) const;
    bool check(const Feature& other) const;

    bool hasEpsilons() const { return epsilons.size(); }

    bool operator<(const Feature& other) const;

    bool hasSameEpsilons(const Feature& other) const
    {
        return epsilons == other.epsilons;
    }

    Eigen::Vector3f deriv;

protected:
    boost::container::small_vector<Eigen::Vector3f, 4> epsilons;
};

}   // namespace libfive
