#pragma once

#include <Eigen/Eigen>

#include "ao/eval/eval_deriv.hpp"
#include "ao/eval/feature.hpp"

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
     *  Checks for features at the given position
     */
    std::list<Feature> featuresAt(const Eigen::Vector3f& p);
};

}   // namespace Kernel
