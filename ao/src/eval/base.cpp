#include <iostream>

#include "ao/eval/base.hpp"

namespace Kernel {

BaseEvaluator::BaseEvaluator(std::shared_ptr<Tape> t,
                             const std::map<Tree::Id, float>& vars)
    : tape(t)
{
    for (auto& v : t->vars.right)
    {
        if (vars.find(v.first) == vars.end())
        {
            std::cerr << "BaseEvaluator::BaseEvaluator: "
                      << "uninitialized variable." << std::endl;
        }
    }
}

}   // namespace Kernel
