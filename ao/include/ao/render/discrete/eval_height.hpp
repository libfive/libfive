#pragma once

#include "ao/eval/eval_deriv_array.hpp"
#include "ao/eval/eval_interval.hpp"
#include "ao/eval/tape.hpp"

namespace Kernel {

class HeightmapEvaluator
{
public:
    HeightmapEvaluator(const Tree t)
        : tape(new Tape(t)), array(tape), interval(tape)
    { /* Nothing to do here */ }

protected:
    std::shared_ptr<Tape> tape;

public:
    DerivArrayEvaluator array;
    IntervalEvaluator interval;
};

}   // namespace Kernel
