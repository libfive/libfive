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
#pragma once

#include <boost/container/small_vector.hpp>
#include <Eigen/Eigen>

namespace Kernel
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

    /*
     *  Checks to see whether the given epsilon is compatible with
     *  the existing epsilon vector.
     */
    bool check(const Eigen::Vector3f& e, bool* dup=nullptr) const;
    bool check(const Feature& other) const;

    Eigen::Vector3f deriv;

protected:
    boost::container::small_vector<Eigen::Vector3f, 4> epsilons;
};

}   // namespace Kernel
