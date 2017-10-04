#pragma once

#include <Eigen/Eigen>

#include "ao/eval/eval_d.hpp"
#include "ao/eval/feature.hpp"

namespace Kernel {

class FeatureEvaluator : public DerivArrayEvaluator
{
public:
    FeatureEvaluator(Tape& t);
    FeatureEvaluator(Tape& t, const std::map<Tree::Id, float>& vars);

protected:
    /*  ambig(index) returns whether a particular slot is ambiguous */
    Eigen::Array<bool, 1, N> ambig;

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

    /*
     *  Returns a list of ambiguous items from indices 0 to i
     *
     *  This call performs O(i) work to set up the ambig array
     */
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(size_t i);

    /*
     *  Checks whether the given position is ambiguous
     */
    bool isAmbiguous(const Eigen::Vector3f& p);
};

}   // namespace Kernel
