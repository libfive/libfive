#pragma once

#include "ao/eval/eval_deriv_array.hpp"
#include "ao/eval/eval_interval.hpp"
#include "ao/eval/eval_feature.hpp"

namespace Kernel {

class XTreeEvaluator
{
public:
    XTreeEvaluator(const Tree t)
        : XTreeEvaluator(t, std::map<Tree::Id, float>())
    { /* Nothing to do here */ }

    XTreeEvaluator(const Tree t, const std::map<Tree::Id, float>& vars)
        : tape(new Tape(t)), array(tape, vars),
          interval(tape, vars), feature(tape, vars)
    { /* Nothing to do here */ }

protected:
    std::shared_ptr<Tape> tape;

public:
    DerivArrayEvaluator array;
    IntervalEvaluator interval;
    FeatureEvaluator feature;
};

}   // namespace Kernel
