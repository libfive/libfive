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
     *  Checks for features at the given position.
     */
    std::list<Feature> featuresAt(const Eigen::Vector3f& p);
};

}   // namespace Kernel
