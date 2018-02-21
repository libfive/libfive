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

#include <Eigen/Eigen>
#include <boost\container\small_vector.hpp>

#include "libfive/eval/eval_deriv.hpp"
#include "libfive/eval/feature.hpp"

namespace Kernel {

class FeatureEvaluator : public DerivEvaluator
{
public:
    FeatureEvaluator(std::shared_ptr<Tape> t);
    FeatureEvaluator(std::shared_ptr<Tape> t,
                     const std::map<Tree::Id, float>& vars);

public:
    /*
     *  Pushes into a tree based on the given feature
     *
     *  The result data f[][0] must contain evaluation results with a matching
     *  number of ambiguous min/max nodes as the given feature (or more, if
     *  the feature is partial)
     *
     *  Returns the feature f with non-relevant choices removed
     */
    Feature push(const Feature& f);

    /*
     *  Checks to see if the given point is inside the solid body.
     *  There are three cases
     *      eval(x, y, z) < 0  => true
     *      eval(x, y, z) > 0  => false
     *      eval(x, y, z) == 0 => further checking is performed
     */
    bool isInside(const Eigen::Vector3f& p);

    /*
     *  Checks for features at the given position.  Also sets non-ambiguous
     *  derivatives, allowing the two-argument version of deriv to be called.
     */
    std::list<Feature> featuresAt(const Eigen::Vector3f& p);

    /*  Non-virtual override that writes to dOrAll as well.
    */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);

    /*  Uses gradients for oracles from a feature, where they exist, rather 
     *  than just taking the first one.  Does not recalculate non-ambiguous 
     *  oracle results; the one-argument version of deriv needs to be called
     *  first to populate those values (this is done in the process of getting
     *  features from featuresAt).  If feature has no non-ambiguous oracle 
     *  results, functions as the one-argument version.
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt, const Feature& feature);

protected:
    /*  Stores all derivatives of oracle results, and where to use them, rather
     *  than just the first.
     */
    Eigen::Array<
        boost::container::small_vector<
        std::pair<Eigen::Vector3f, std::vector<Eigen::Vector3f>>, 1>, 
        1, Eigen::Dynamic> dOrAll;
};

}   // namespace Kernel
