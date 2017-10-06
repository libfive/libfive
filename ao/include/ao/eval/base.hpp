#pragma once

#include "ao/eval/tape.hpp"

namespace Kernel {

class BaseEvaluator
{
public:
    BaseEvaluator(std::shared_ptr<Tape> t,
                  const std::map<Tree::Id, float>& vars);

    /*
     *  Pops the tape
     *  (must be paired against evalAndPush)
     */
    void pop() { tape->pop(); }

protected:
    std::shared_ptr<Tape> tape;

};

}   // namespace Kernel
